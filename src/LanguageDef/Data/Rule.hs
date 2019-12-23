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

import Lens.Micro (Lens')

{-| 

A predicate represents something that can be calculated and either is successfull or not.

If an expression is given (an applied function), then the function is evaluated. If evaluation is sucessfull, the expression is considered 'true'
If a 'conclusion' is given, that predicate will be attempted to be calculated

-}
data Predicate' a
	= PredConcl	{_predConcl	:: Conclusion' a, _predLocation	:: LocationInfo}
	| PredExpr	{_predExpr	:: Expression' a, _predLocation	:: LocationInfo}
	deriving (Show, Eq)

predEither	:: (Conclusion' a -> b) -> (Expression' a -> b) -> Predicate' a -> b
predEither fc _ (PredConcl concl _)
	= fc concl
predEither _ fe (PredExpr expr _)
	= fe expr

instance Functor Predicate' where
	fmap f (PredConcl concl li)
		= PredConcl (concl |> f) li
	fmap f (PredExpr expr li)
		= PredExpr (expr |> f) li

instance ToString (Predicate' a) where
	toParsable (PredConcl concl _)	= toParsable concl
	toParsable (PredExpr expr _)	= toParsable expr

	toCoParsable (PredConcl concl _)
					= toCoParsable concl
	toCoParsable (PredExpr expr _)	= toCoParsable expr

	debug (PredConcl concl _)	= debug concl
	debug (PredExpr expr _)		= debug expr

	

type Predicate	= Predicate' SyntFormIndex


{-|

 A conclusion is the part of a predicate, and contains an pattern for each parameter (either in or out)

-}
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
		= Rule (preds ||>> f)
			(concl |> f)
			nm
			docs

makeLenses ''Predicate'
makeLenses ''Conclusion'
makeLenses ''Rule'

ruleAbout	:: Lens' (Rule' a) FQName
ruleAbout
	= ruleConcl . conclRelName


instance ToString (Conclusion' a) where
	toParsable (Conclusion nm args)
		= inParens (showFQ nm) ++ " " ++ (args |> toParsable & commas)


instance ToString (Rule' a) where
	toParsable (Rule preds concl nm docs)
		= let	doc'	= toParsable docs
		 	preds'	= preds |> toParsable
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

