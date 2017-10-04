{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module LanguageDef.Utils.Checkable where

{- Implements something that can be checked -}

import Utils.All

import LanguageDef.Utils.ExceptionInfo


type Check	= Failable ()


class Checkable a where
	check	:: a -> Check
	check a	= return ()


class Checkable' info a where
	check'	:: info -> a -> Check
	check' info a
		= return ()


checkM	:: Checkable a => Maybe a -> Check
checkM	= maybe pass check


checkM'	:: Checkable' x a => x -> Maybe a -> Check
checkM'	x
	= maybe pass (check' x)



checkNoDuplicates	:: (Eq a) => [a] -> ([a] -> String) -> Check
checkNoDuplicates as msg
	= do	let dups	= dubbles as
		assert' (null dups) $ msg dups 
