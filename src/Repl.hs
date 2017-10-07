{-# LANGUAGE TemplateHaskell #-}
module Repl where

{- An interactive Repl -}

import Utils.All

import LanguageDef.API
import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.LocationInfo

import qualified Utils.PureIO as PureIO
import Utils.PureIO (runIO)

import System.IO
import System.Console.ANSI
import Text.PrettyPrint.ANSI.Leijen (text, red, yellow, bold, dullgreen, green)
import Utils.GetLine

import Data.List
import Data.Char

import Graphs.Lattice (debugLattice)

import Control.Monad.State hiding (get)




data ReplState	= ReplState 
	{ _currentModule	:: LDScope
	, _currentPath		:: FilePath
	, _currentModulePath	:: [FilePath]
	}
makeLenses ''ReplState


type Action	= StateT ReplState IO

trepl	= repl "/home/pietervdvn/git/ALGT2/src/Assets" ["STFL"]

repl		:: FilePath -> [Name] -> IO ()
repl fp path
	= do	ld	<- runIO (loadLangDef fp path) |> crash
		let state	= ReplState ld fp path
		runStateT _repl state
		pass

_repl		:: Action ()
_repl	= do	line	<- liftIO $ prompt "※  "
		if null $ stripL line then
			_repl
		else do
			let foundActions	= actions _repl & filter (\(keys, _) -> keys & any (`isPrefixOf` line))
			if null foundActions then do
				interpret line
				_repl
			else do
				let matchingString	= head foundActions & fst & filter (`isPrefixOf` line) & head	:: String
				let cleanString	= stripL $ drop (length matchingString) line
				(head foundActions & snd) cleanString
		


-- Mapping of prefix -> action. Some actions might pass control flow to the given continuation
actions	:: Action () -> [([String], String -> Action ())]
actions continuation
 	= let continue action str	= action str >> continuation in
		[ ([":l", "\f"]			, continue (noArg clearScreenAct))
		, ([":quit",":q", "\EOT"]	, const pass)
		, ([":r"]			, continue $ noArg reload)
		, ([":*"]			, continue $ noArg infoAboutAll)
		, ([":i"]			, continue infoAboutAct)
		, ([":st"]			, continue $ noArg supertypeInfo)
		, (["help", ":h", ":help"]	, continue help)
		, (["\ESC[A", "\ESC[B", "\ESC[C", "\ESC[D"]
						, continue $ const pass)	-- We ignore going up, down, left 
		, (["\ESC"]			, continue printEscCode)
		]



help	:: String -> Action ()
help _	= let	cmds	= actions (error "hi") |> fst |> filter (all isPrint) |> commas & filter (not . null)	:: [String] in
		putStrLn' $ "Supported commands are:\n"++ (cmds & unlines & indent)


printEscCode	:: String -> Action ()
printEscCode msg
	= do	putColored' red "Unhandled escape code"
		liftIO $ print msg


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
		liftIO $ handleFailure printPars printPars $ runPredicate' ld "<interactive>" line

reload	:: Action ()
reload
	= do	ld	<- getLd
		fp	<- gets' currentPath
		fq	<- gets' currentModulePath
		putStrLn' ("Reloading " ++ fp ++ "/" ++ intercalate "/" fq)
		mLd	<- liftIO $ runIO (loadLangDef fp fq)
		let recover e	= do	liftIO $ printPars e
					putStrLn' "Using the old definition"
		mLd & handleFailure recover (puts' currentModule)			


noArg	:: Action () -> String -> Action ()
noArg action ""
	= action
noArg action str
	= do	putColored' yellow $ "No argument is required for this action; the argument is ignored"
		action



putColored color msg
	= putStrLn $ show $ color $ text msg

putColored' c m
	= liftIO $ putColored c m

putStrLn'
	= liftIO . putStrLn

gets' lens
	= gets (get lens)

getLd	= gets' currentModule

puts' lens x
	= modify (set lens x)
