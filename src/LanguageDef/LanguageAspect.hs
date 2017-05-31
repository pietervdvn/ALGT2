module LanguageDef.LanguageAspect where

{- 

A language aspect is a part of the language, such as syntax, semantics, ...

It is an enclosed module, but might require other aspects to function

 -}

import Utils.Utils

import Utils.ToString





class (Show la) => LanguageAspect la where
	name	:: la -> String
	author	:: la -> String
	version	:: la -> [Int]
	dependencies
		:: la -> [(String, String)]
