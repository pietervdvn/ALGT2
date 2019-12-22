{-# LANGUAGE TemplateHaskell #-}
module Repl where

{- An interactive Repl, based on the shell -}

import Utils.All
import Assets

import LanguageDef.API
import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.LocationInfo

import LanguageDef.Data.Expression
import LanguageDef.Data.LanguageDef
import LanguageDef.Data.ParseTree
import LanguageDef.Data.SyntFormIndex

import qualified Utils.PureIO as PureIO
import Utils.PureIO (runIO)

import System.IO
import System.Console.ANSI
import Text.PrettyPrint.ANSI.Leijen (text, red, yellow, bold, dullgreen, green, onred)
import qualified Text.PrettyPrint.ANSI.Leijen as Ansi
import Utils.GetLine

import Data.List
import Data.Char
import Data.Maybe
import Data.Time.Clock

import Graphs.Lattice (debugLattice)

import Control.Monad.State hiding (get)
import qualified Data.Map as M
import Data.Map (Map)


data ReplState	= ReplState 
	{ _currentModule	:: Maybe LDScope
	, _currentPath		:: FilePath
	, _currentModulePath	:: [FilePath]
	, _currentPromptMsg	:: String -> String
	}
makeLenses ''ReplState


type Action	= StateT ReplState IO

trepl	= replAsset "" ["ALGT", "Sugared", "Syntax"]


replSTFL
	= replAsset "TestLanguages" ["STFL"]

replAsset fp
	= repl $ "src/Assets/" ++ fp

repl		:: FilePath -> [Name] -> IO ()
repl fp path
	= do	let state	= ReplState Nothing fp path (show . onred . text)
		runStateT (reload >> _repl) state
		pass



_repl		:: Action ()
_repl	= do	promptMsg	<- gets' currentPromptMsg
		ld	<- gets' currentModule
		line	<- liftIO $ prompt' ("※  ", putStr . promptMsg)
		liftIO $ putStrLn ""
		if isNothing ld && line /= "\EOT" then do
			reload
			_repl
		else if null $ stripL line then
			_repl
		else do
			let foundActions	= actions _repl & filter (\(keys, _, _) -> keys & any (`isPrefixOf` line))
			if null foundActions then do
				interpret line
				_repl
			else do
				let matchingString	= head foundActions & fst3 & filter (`isPrefixOf` line) & head	:: String
				let cleanString	= stripL $ drop (length matchingString) line
				(head foundActions & snd3) cleanString
		


-- Mapping of prefix -> action. Some actions might pass control flow to the given continuation
actions	:: Action () -> [([String], String -> Action (), String)]
actions continuation
 	= let continue action str	= action str >> continuation in
		[ ([":l", "\f", "CTRL+L"]			, continue (noArg clearScreenAct),
			"Clears the screen")
		, ([":quit",":q", "\EOT", "CTRL+D"]	, const pass,
			"Exit the interpreter")
		, ([":r"]			, continue $ noArg reload,
			"Reload the language definition")
		, ([":*"]			, continue $ noArg infoAboutAll,
			"Show what is in scope")
		, ([":i"]			, continue infoAboutAct,
			"Give info about an entity in scope (needs an argument)")
		, ([":t"]			, continue printType,
			"Give the type of an expression (needs an argument)")	
		, ([":st"]			, continue $ noArg supertypeInfo,
			"Gives the supertyping relationship")
		, ([":pt"]			, continue (parseTreeInfo "interactive"),
			"Gives the parsetree of an expression")
		, (["help", ":h", ":help"]	, continue help,
			"Print this help message")
		]



help	:: String -> Action ()
help _	= putStrLn' $ generalHelp ++ (actions (error "no continuation") |> showAction & unlines & indent)


generalHelp = Assets._Resources_InterpreterHelp_md


showAction	:: ([String], a, String) -> String
showAction (comms, _, help)
	= let	comms'	= comms |> filter isPrint & filter (not . null) |> text |> bold |> show in
		commas comms' ++ "\t"++help

printType	:: String -> Action ()
printType str
	= do	ld	<- getLd
		let typed	= createTypedExpression ld "interactive" str typeTop	:: Failable Expression
		typed & handleFailure (putStrLn' . toParsable) 
			(\expr -> expr & get expAnnot & toParsable & putStrLn')


parseTreeInfo	:: String -> String -> Action ()
parseTreeInfo filename str
	= do	ld	<- getLd
		let expr        = createExpression ld filename str >>= extractPT        :: Failable String
		expr & handleFailure (putStrLn' . toParsable) (\str ->
			do	
				let typesToTry = allKnowns ld syntaxCall |> fst3 |> fst :: [FQName]
				let parses = typesToTry |> (\fqn -> (fqn, createParseTree ld fqn "interactive-pt" str)) |> sndEffect
				let results = successess parses
				if null results then do
					("Could not parse "++str) & putStrLn'
					let failed = fails parses
					failed |> toParsable & unlines & putStrLn'
				else 
					results |> (\(typ, parseTree) ->"\n" ++ showFQ typ++"\n"++asTree parseTree) & unlines & putStrLn'
			)

parseFile   :: String -> Action ()
parseFile str
	= do	let args = words str
		if length args /= 2 then
			putStrLn' "Not enough arguments"
		else do
			let rule = head args 
			let path = args !! 1
			exists <- liftIO $ runIO $ PureIO.doesFileExist path
			if not exists then
				putStrLn' $ "File does not exist: "++path
			else do
				fileContents <- liftIO $ readFile path
				ld <- getLd
				(do	fqn <- resolve ld syntaxCall ([], rule)
					pt 	<- createParseTree ld fqn path fileContents
					return (fqn, pt))
					& handleFailure (putStrLn' . toParsable) (putStrLn' . asTree . snd)
					
				

extractPT		:: Expression' () -> Failable String
extractPT (ParseTree (Literal ptToken _ _ _) _ _)
					= return ptToken
extractPT _		= fail "This is not a parsetree"

clearScreenAct	:: Action ()
clearScreenAct
	= liftIO clearScreen 


supertypeInfo	:: Action ()
supertypeInfo
	= do	lattice	<- getLd |> supertypes
		putColored' dullgreen $ debugLattice (show . yellow . text . showFQ) lattice


infoAboutAll	:: Action ()
infoAboutAll
	= do	ld	<- getLd
		let scoped	= inScope ld |> fst
		scoped |> showInScope |+> putStrLn'
		pass



showInScope	:: (FQName, [FQName]) -> String
showInScope (full, knownAs)
	= let	full'		= full & showFQ & text & bold & show
		knownAs'	= knownAs |> showFQ |> indent & intercalate "\n"
		in
		full' ++ " is known as:\n" ++ knownAs'
				

infoAboutAct	:: String -> Action ()
infoAboutAct entry
	= do	ld	<- getLd
		let fq	= uncalate '.' entry
		let infos	= infoAbout' ld fq
		when (null infos) $ putColored' red "Nothing found"
		let infos'	= infos |> uncurry toParsable'
					 & unlines
		liftIO $ putStrLn infos'


interpret	:: String -> Action ()
interpret line
	= do	ld	<- getLd
		let failed err	= do	setColor yellow
					liftIO $ printPars err
		let success exp	= do	setColor green
					liftIO $ printPars exp
		handleFailure failed success $ runPredicate' ld "<interactive>" line

reload	:: Action ()
reload
	= do	fp	<- gets' currentPath
		fq	<- gets' currentModulePath
		putStrLn' ("Reloading " ++ fp ++ "/" ++ intercalate "/" fq)
		mLd	<- liftIO $ runIO (loadLangDef fp fq)
		let recover e	= do	putStrLn' $ toParsable e
					old	<- gets' currentModule
					setColor $ if isNothing old then onred else red
					putStrLn' "Using the old definition"

		let success ld	= do	setColor green
					puts' currentModule $ Just ld
					putStrLn' "Loading successfull!"

		mLd & handleFailure recover success			


noArg	:: Action () -> String -> Action ()
noArg action ""
	= action
noArg action str
	= do	putColored' yellow "No argument is required for this action; the argument is ignored"
		action


setColor	:: (Ansi.Doc -> Ansi.Doc) -> Action ()
setColor color
	= puts' currentPromptMsg (show . color . text)
					

putColored color msg
	= print $ color $ text msg

putColored' c m
	= liftIO $ putColored c m

putStrLn'	:: String -> Action ()
putStrLn'
	= liftIO . putStrLn

gets' lens
	= gets (get lens)


puts' lens x
	= modify (set lens x)
	

				


-- Gets the loaded module
getLd	:: Action LDScope
getLd	= do	mld	<- gets' currentModule
		maybe (fail "Initial load of the language definition failed") return mld


