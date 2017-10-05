{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, DeriveFunctor #-}
module LanguageDef.Data.Rule where

import Utils.Utils

import Utils.All
import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.Grouper
import LanguageDef.Utils.ExceptionInfo


import LanguageDef.Data.Expression hiding (choices')
import LanguageDef.Data.SyntFormIndex
	

import Data.Map (Map, (!), filterWithKey)
import qualified Data.Map as M

type Predicate' a
	= Either (Conclusion' a) (Expression' a)

type Predicate	= Predicate' SyntFormIndex

data Conclusion' a
	= Conclusion
		{ _conclRelName	:: FQName
		, _conclArgs	:: [Expression' a]
		} deriving (Show, Eq, Functor)

type Conclusion	= Conclusion' SyntFormIndex

data Rule' a
	= Rule	{ _rulePreds	:: [Predicate' a]
		, _ruleConcl	:: Conclusion' a
		, _ruleName	:: Name
		, _ruleDocs	:: MetaInfo
		} deriving (Show, Eq)

type Rule	= Rule' SyntFormIndex

instance Functor Rule' where
	fmap f (Rule preds concl nm docs)
		= Rule (preds |> either (\concl -> concl |> f & Left)
					(\expr -> expr |> f & Right))
			(concl |> f)
			nm
			docs

makeLenses ''Conclusion'
makeLenses ''Rule'


instance ToString (Conclusion' a) where
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

