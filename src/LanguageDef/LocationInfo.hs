{-# LANGUAGE RankNTypes, TemplateHaskell #-}
module LanguageDef.LocationInfo where

{-Small helper data structure, containg start and end position of parsetrees -}

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


data MetaInfo	=  MetaInfo
	{ _miLoc	:: LocationInfo
	, _miDoc	:: Doc}
	deriving (Show, Ord, Eq)
makeLenses ''MetaInfo


instance ToString MetaInfo where
	toParsable meta
		= if null $ get miDoc meta then "" else meta & get miDoc & lines |> ("# "++) & intercalate "\n"	-- block comments
	toCoParsable meta
		= if null $ get miDoc meta then "" else meta & get miDoc & lines & concat & ("\t # "++)




atLocation	:: LocationInfo -> Either String a -> Either String a
atLocation
	= inMsg . toParsable

unknownLocation	:: LocationInfo
unknownLocation 
	= LocationInfo (-1) (-1) (-1) (-1) ""


locationSpec	:: LocationInfo -> String
locationSpec (LocationInfo sl el sc ec file)
 | any (== (-1)) [sl, el, sc, ec]
	= "unspecified location"
 | sl == el && sc == ec
	= "line "++show sl++", "++show sc
 | sl == el
	= "line "++show sl++", column "++show sc++" - "++show ec
 | otherwise
	= "lines "++show sl++" - "++show el


instance ToString LocationInfo where
	toParsable li	= "In "++(show $ get miFile li)++" "++inParens (locationSpec li)
	toCoParsable li	= " at " ++ locationSpec li ++", "++ show (get miFile li)
