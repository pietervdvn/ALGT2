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


-- Test language as LangDefs
testLangDefs	:: LangDefs
testLangDefs
	= loadAll "" ["TestLanguage"] & runPure allAssets' & either error fst

testLDScope
	= let	(LangDefs x)	= testLangDefs
		in x ! ["TestLanguage"]


