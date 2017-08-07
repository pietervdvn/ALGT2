module LanguageDef.API where

{- 

Creates all kinds of stuff based on strings, to test with, such as expressions of functions, parsetrees, ...

This should all be based on some syntax

 -}

import Utils.All
import Utils.PureIO

import AssetUtils

import LanguageDef.LanguageDef
import LanguageDef.Syntax.All
import LanguageDef.LocationInfo
import LanguageDef.ModuleLoader
import LanguageDef.MetaExpression
import qualified LanguageDef.Syntax.Combiner as Combiner
import LanguageDef.FunctionTyper
import LanguageDef.MetaFunction

import LanguageDef.LangDefs


loadLangDef :: FilePath -> [Name] -> PureIO LangDefs
loadLangDef	= loadAll

loadAssetLangDef	:: FilePath -> [Name] -> Either String LangDefs
loadAssetLangDef fp names
		= runPure allAssets' (loadLangDef fp names) |> fst

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
