module AssetUtils where

import Assets

import Data.Map as M


allAssets'
	= let	noPrefix	= M.fromList allAssets
		slashPrefix	= M.mapKeys ("/"++) $ M.fromList allAssets in
		([], M.union noPrefix slashPrefix)

