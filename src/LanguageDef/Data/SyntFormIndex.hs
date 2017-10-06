{-# LANGUAGE TemplateHaskell #-}
module LanguageDef.Data.SyntFormIndex where

import Utils.All

import LanguageDef.Utils.LocationInfo


import Data.List as L

type SyntForm	= FQName
data SyntFormIndex = SyntFormIndex
			{ _syntIndForm	:: SyntForm
			, _syntIndChoice	:: Int	-- To what choice does it correspond?
			, _syntIndSeqInd	:: Maybe Int	-- To what index in the sequence of the choice does it correspond?
			} 
			| NoIndex { _syntIndForm	:: SyntForm}
			deriving (Show, Eq)

makeLenses ''SyntFormIndex


removeIndex'	:: SyntFormIndex -> SyntFormIndex
removeIndex' (SyntFormIndex sf _ _)
	= NoIndex sf
removeIndex' noIndex	= noIndex

removeIndex i	= i & get syntIndForm

-- Selects all elements in the list, for which the syntactic form is a smallest type
selectSmallest	:: (FQName -> FQName -> Bool) ->  [(a, SyntFormIndex)] -> [(a, SyntFormIndex)]
selectSmallest isSubtypeOf
	= _selectSmallest isSubtypeOf []

_selectSmallest	:: (FQName -> FQName -> Bool) -> [(a, SyntFormIndex)] -> [(a, SyntFormIndex)] -> [(a, SyntFormIndex)]
_selectSmallest isSubtypeOf [] (a:as)
		= _selectSmallest isSubtypeOf [a] as
_selectSmallest _ smallest []	= smallest
_selectSmallest isSubtypeOf smallest (a@(_, toJudge):as)
 -- if to judge is a subtype of a value from smallest:
 | any (isSubtypeOf (get syntIndForm toJudge)) (smallest |> snd |> get syntIndForm)
	= let	toJudgeFQN	= toJudge & get syntIndForm
		smallest'	= L.filter (not . isSubtypeOf toJudgeFQN . get syntIndForm . snd) smallest
		in
		_selectSmallest isSubtypeOf (a:smallest') as
 | otherwise
	= _selectSmallest isSubtypeOf smallest as


instance ToString SyntFormIndex where
	toParsable (NoIndex sf)	= showFQ sf
	toParsable (SyntFormIndex f ch ind)
				= showFQ f ++":"++show ch++(ind |> show |> ("."++) & fromMaybe "")


