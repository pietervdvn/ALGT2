{-# LANGUAGE MultiParamTypeClasses #-}
module LanguageDef.LanguageAspect where

{- 

A language aspect is a part of the language, such as syntax, semantics, ...

It is an enclosed module, but might require other aspects to function

 -}

import Utils.Utils

import Utils.ToString
import LanguageDef.Syntax.Syntax
import LanguageDef.Syntax.ParseTree




class (Show la) => LanguageAspect deps la where
	name	:: deps -> la -> String
	author	:: deps -> la -> String
	version	:: deps -> la -> [Int]
	parsingSyntax	:: deps -> (Syntax, String, Combiner la)
