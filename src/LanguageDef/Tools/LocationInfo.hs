{-# LANGUAGE RankNTypes, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
module LanguageDef.Tools.LocationInfo where

{-Small helper data structure, containg start and end position of parsetrees, metainfo or fully qualified names etc-}

import Utils.All
import Data.Function (on)

type FQName	= ([Name], Name)

showFQ (ns, nm)
		= (ns ++ [nm]) & dots


distFQ	:: (FQName -> String, FQName -> FQName -> Int)
distFQ
	= (showFQ, levenshtein `on` snd)



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
	, _miDoc	:: Doc
	}
	deriving (Show, Ord, Eq)
makeLenses ''MetaInfo


data AllInfo	= AllInfo
	{ _allInfName	:: Name
	, _allInfType	:: Name
	, _allInfMI	:: MetaInfo
	, _allInfRepr	:: String
	} deriving (Show, Ord, Eq)
makeLenses ''AllInfo

class Infoable a where
	getInfo		:: a -> AllInfo

instance ToString' FQName AllInfo where
	toParsable' fqn (AllInfo nm tp mi repr)
		= inHeader' (showFQ fqn ++ " " ++ inParens tp) $
			toParsable mi ++ repr

	toCoParsable'	= toParsable'
	debug'		= toParsable'
	show'		= toParsable'

instance ToString MetaInfo where
	toParsable meta
		= meta & get miDoc & indentWith "# " -- block comments
	toCoParsable meta
		= if null $ get miDoc meta then "" else meta & get miDoc & lines & concat & ("\t # "++)



unknownLocation	:: LocationInfo
unknownLocation 
	= LocationInfo (-1) (-1) (-1) (-1) ""


locationSpec	:: LocationInfo -> String
locationSpec (LocationInfo sl el sc ec file)
 | (-1) `elem` [sl, el, sc, ec]
	= "unspecified location"
 | sl == el && sc == ec
	= "line "++show sl++", "++show sc
 | sl == el
	= "line "++show sl++", column "++show sc++" - "++show ec
 | otherwise
	= "lines "++show sl++" - "++show el


instance ToString LocationInfo where
	toParsable li	= "In "++show (get miFile li)++" "++inParens (locationSpec li)
	toCoParsable li	= " at " ++ locationSpec li ++", "++ show (get miFile li)
