{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, DeriveFunctor #-}
module LanguageDef.Rule where

import Utils.Utils

import Utils.All
import LanguageDef.LocationInfo
import LanguageDef.Syntax.All
import LanguageDef.MetaExpression hiding (choices')
import LanguageDef.Grouper


import Data.Map (Map, (!), filterWithKey)
import qualified Data.Map as M


data Conclusion a
	= Conclusion
		{ _conclRelName	:: FQName
		, _conclArgs	:: [Expression a]
		} deriving (Show, Eq, Functor)

data Rule' a
	= Rule	{ _rulePreds	:: [Either (Conclusion a) (Expression a)]
		, _ruleConcl	:: Conclusion a
		, _ruleName	:: Name
		, _ruleDocs	:: MetaInfo
		} deriving (Show, Eq)

instance Functor Rule' where
	fmap f (Rule preds concl nm docs)
		= Rule (preds |> either (\concl -> concl |> f & Left)
					(\expr -> expr |> f & Right))
			(concl |> f)
			nm
			docs

makeLenses ''Conclusion
makeLenses ''Rule'

type Rule	= Rule' ()


instance ToString (Conclusion a) where
	toParsable (Conclusion nm args)
		= inParens (showFQ nm) ++ " " ++ (args |> toParsable & commas)


instance ToString (Rule' a) where
	toParsable (Rule preds concl nm docs)
		= let	doc'	= toParsable docs
		 	preds'	= preds |> either toParsable toParsable
					& intercalate "\t"
			concl'	= toParsable concl
			dashesL	= 1 + max (length preds') (length concl')
			dashes	= replicate dashesL '-'
			in
			[ doc'
			, " " ++ preds'
			, "-" ++ dashes ++ " [ "++nm++" ]"
			, " " ++ concl'
			] & unlines 


instance Infoable (Rule' a) where
	getInfo r
		= AllInfo (get ruleName r) "Rule" (get ruleDocs r) (toParsable r)

