module LanguageDef.DebugAPI where

{-
This module contains tools for Pietervdvn to debug stuff
-}

import Utils.All

import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.Grouper

import LanguageDef.Data.SyntacticForm
import LanguageDef.Data.ParseTree
import LanguageDef.Data.LanguageDef

import LanguageDef.MetaSyntax

import Data.Map as M
import Data.List as L
import Data.Maybe

import System.Directory

import Control.Monad


		
saveMetaSyntaxes	:: IO ()
saveMetaSyntaxes
	= do	putStrLn "Saving metasyntaxes to the asset files..."
		dir	<- getCurrentDirectory |> (++"/src/Assets/")
		putStrLn $ "Directory is: "++dir
		dirExists	<- doesDirectoryExist dir
		when dirExists $ removeDirectoryRecursive dir
		createDirectory dir
		metaSyntaxes & M.toList |+> uncurry (_saveMetaSyntax dir)
		pass


_saveMetaSyntax	:: FilePath -> [Name] -> Syntax -> IO ()
_saveMetaSyntax dir nm syntax
	= do	let target	= dir ++"/" ++ dots nm ++ ".language"
		print target
		let imports	= metaSyntaxes & M.keys |> (["ALGT","Native"] ++) |> dots |> ("import "++) & unlines
		let contents	= imports ++ 
				  inHeader " " (dots nm) '*' 
				  ("# Automatically generated; do not edit" ++ 
				  inHeader' "Syntax"
				  (toParsable syntax))
		writeFile target contents


-- Tests parsing of a rule against a given string
testSyntax	:: Name -> String -> Failable ParseTree
testSyntax rule string
	= do	
		let knownNames	= metaSyntaxes & M.elems |> get grouperOrder & concat & sort :: [Name]
		assert' (rule `elem` knownNames) $ "The rule "++show rule++" is not defined. Instead, you could try:\n"++unlines knownNames
		let foundRules	= metaSyntaxes |> get grouperDict |> M.lookup rule
					& M.filter isJust |> fromJust	:: Map [Name] SyntacticForm
		let foundKeys	= M.keys foundRules
		assert' (length foundKeys < 2) $ "The rule was found in multiple syntaxes: "++(foundKeys |> dots & commas)
		let syntForm	= foundRules ! head foundKeys
		parse "DebugAPI.testSyntax" (metaSyntaxes, head foundKeys) rule string

{-
do	let found	= metaSyntaxes & M.filter (\s -> rule `M.member` get grouperDict s) & M.keys
		unless (L.null found) $ error $
			"No rule "++rule++" does exist within any syntax\n" ++ (metaSyntaxes |> get grouperDict |> M.keys & concat & commas) 
		when (length found > 1) $ error $ "Rule "++rule ++" does exist in "++(found |> dots & commas)
		let fqn	= found & head
		putStrLn $ showFQ (fqn, rule)
		let parsed	=  parse "testSyntax" (metaSyntaxes, fqn) rule string
					& legacy & either error id
		printPars parsed
-}
