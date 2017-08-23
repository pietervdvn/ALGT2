module LanguageDef.API (loadLangDef, loadAssetLangDef, createParseTree, createExpression, createTypedExpression, resolveAndRun', parseTarget) where

{- 

Creates all kinds of stuff based on strings, to test with, such as expressions of functions, parsetrees, ...

This should all be based on some syntax

 -}

import Utils.All
import Utils.PureIO

import AssetUtils

import LanguageDef.LanguageDef
import LanguageDef.Syntax.All as Syntax
import LanguageDef.LocationInfo
import LanguageDef.ModuleLoader
import LanguageDef.MetaExpression
import qualified LanguageDef.Syntax.Combiner as Combiner
import LanguageDef.FunctionTyper
import LanguageDef.MetaFunction

import LanguageDef.FunctionInterpreter

import LanguageDef.LangDefs

{- Loads a language definition from the filesystem; use with 'runIO'-}
loadLangDef :: FilePath -> [Name] -> PureIO LangDefs
loadLangDef	= loadAll

{- Loads a language definition directly from the assets; is Pure -}
loadAssetLangDef	:: FilePath -> [Name] -> Either String LangDefs
loadAssetLangDef fp names
		= runPure allAssets' (loadLangDef fp names) |> fst


{- parseTarget: creates a parsetree based on the syntax of langdefs -}
-- parseTarget

createParseTree		:: LangDefs -> FQName -> FilePath -> String -> Either String ParseTree'
createParseTree
	= parseTarget 

createExpression	:: LangDefs -> FilePath -> String -> Either String (Expression LocationInfo)
createExpression ld source str
	= do	pt	<- parse source (metaSyntaxes, ["Functions"]) "expression" str
		Combiner.interpret expression pt

createTypedExpression	:: LangDefs -> FilePath -> String -> FQName -> Either String (Expression (LocationInfo, SyntFormIndex))
createTypedExpression ld source str typ@(loc, nm)
	= do	expr	<- createExpression ld source str
		scope	<- checkExists loc (get langdefs ld)
				("Module "++dots loc++ " not found")
		typeExpression scope typ expr



------------------ Testing codee
-- TODO REMOVE THIS

t' rule str
	= do	def	<- loadLangDef "/home/pietervdvn/git/ALGT2/src/Assets" ["RelationSyntax"]
	--	toParsable def & Utils.PureIO.putStrLn
		let parsed'	= parseTarget def (["RelationSyntax"], rule) "TestFile" str
		parsed	<- parsed' & either error return
		Utils.PureIO.putStrLn $ toParsable parsed
		Utils.PureIO.putStrLn $ toCoParsable parsed
		Utils.PureIO.putStrLn $ debug parsed




t	= runIO $ t' "relDeclarationCore" "(→*) : expr (in) * expr (out) ; Pronounced as \"Bigstep\""
