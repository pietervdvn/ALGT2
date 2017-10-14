module AssetUtils where

import Assets

import Utils.All
import Utils.PureIO

import Data.Map as M

import LanguageDef.ModuleLoader
import LanguageDef.LangDefs

allAssets'
	= let	noPrefix	= M.fromList allAssets
		slashPrefix	= M.mapKeys ("/"++) $ M.fromList allAssets 
		files		= M.union noPrefix slashPrefix
					|> (flip (,) Assets.timeCreated)
		in
		([], files, Assets.timeCreated)



