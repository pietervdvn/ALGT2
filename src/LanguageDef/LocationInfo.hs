{-# LANGUAGE RankNTypes, TemplateHaskell #-}
module LanguageDef.LocationInfo where

import Utils.All



data LocationInfo	= LocationInfo
	{ _liStartLine	:: Int
	, _liEndLine	:: Int
	, _liStartColumn:: Int
	, _liEndColumn	:: Int
	, _miFile	:: String
	}
	deriving (Show, Ord, Eq)
makeLenses ''LocationInfo

atLocation	:: LocationInfo -> Either String a -> Either String a
atLocation
	= inMsg . toParsable



locationSpec	:: LocationInfo -> String
locationSpec (LocationInfo sl el sc ec file)
 | any (== (-1)) [sl, el, sc, ec]
	= "(unspecified location)"
 | sl == el && sc == ec
	= "(line "++show sl++", "++show sc++")"
 | sl == el
	= "(line "++show sl++", column "++show sc++" - "++show ec++")"
 | otherwise
	= "(lines "++show sl++" - "++show el++")"


instance ToString LocationInfo where
	toParsable li	= "In "++(show $ get miFile li)++" "++locationSpec li

