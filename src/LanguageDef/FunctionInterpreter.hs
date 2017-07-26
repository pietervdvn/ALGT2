module LanguageDef.FunctionInterpreter where

{- Interprets functions -}

import Utils.All

import LanguageDef.LanguageDef
import LanguageDef.Syntax

import Data.Map

type VariableStore
	= Map Name (ParseTree', [Int])




