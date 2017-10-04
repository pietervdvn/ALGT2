module AssetUtils where

import Assets

import Utils.All
import Utils.PureIO

import Data.Map as M

import LanguageDef.ModuleLoader
import LanguageDef.LangDefs


allAssets'
	= let	noPrefix	= M.fromList allAssets
		slashPrefix	= M.mapKeys ("/"++) $ M.fromList allAssets in
		([], M.union noPrefix slashPrefix)


{-
testType	:: Name -> FQName
testType t	= (["TestLanguage"], t)

testLDScope
	= getScope ["TestLanguage"]
-- Test language as LangDefs
testLangDefs	:: LangDefs
testLangDefs
	= getLangDefs ["TestLanguage"]

getScope	:: [Name] -> LDScope
getScope nm
	= getLangDefs nm & get langdefs & (! nm)

getLangDefs'	:: [Name] -> Either String LangDefs
getLangDefs' nm
	= loadAll "" nm & runPure allAssets' |> fst

getLangDefs nm
	= getLangDefs' nm & either error id
-}

