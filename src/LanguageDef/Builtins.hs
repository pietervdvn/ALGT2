module LanguageDef.Builtins where

{- 

This module aggregates all builtin stuff

 -}

import Utils.All

import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Data.SyntFormIndex

import LanguageDef.MetaSyntax (typeTop, typeBottom)
import LanguageDef.Data.SyntacticForm
import LanguageDef.Data.ParseTree
import LanguageDef.Data.Function
import LanguageDef.Data.LanguageDef

import Data.Map as M

import Data.List

syntaxExtras	= [ (typeTop, _topSF)
		  , (typeBottom, _bottomSF)
		  ]


biLocations	= LocationInfo 0 0 0 0 "ALGT/Builtins.language"

_bottomSF	:: SyntacticForm
_bottomSF
	= SyntacticForm "⊥" [] [] $ MetaInfo biLocations $
		"The bottom type (⊥) represents values which can not exist (or the intersection of all types). For example, a function which always returns an error; or gets stuck in an infinite loop, is said to have type \"bottom\". An intersection between two types (e.g. \"bool\" ∩ \"int\") does not have a result, and is represented with type bottom"

_topSF		:: SyntacticForm
_topSF	= SyntacticForm "⊤" [] [] $ MetaInfo biLocations $
		"The top type (⊤) represents all possible values (or: the union of all types). For example, a function which accepts any value, will be declared as having ⊤ as input argument"
		



isBuiltinFunction	:: LanguageDef' ResolvedImport fr -> Function' x -> Bool
isBuiltinFunction ld f
 	= "ALGT/Builtins.language" `isSuffixOf` (ld & get (langLocation . miFile))


functions	:: Map FQName ([ParseTree] -> Failable ParseTree) 
functions 	= M.fromList
			[ ((["ALGT", "Builtins"], "plus"), _intOp (+))
			, ((["ALGT", "Builtins"], "min"), _intOp (-))
			, ((["ALGT", "Builtins"], "mul"), _intOp (*))
			, ((["ALGT", "Builtins"], "div"), _intOp div)
			, ((["ALGT", "Builtins"], "mod"), _intOp mod)
			
			, ((["ALGT", "Builtins"], "parse"), _intOp mod)
			, ((["ALGT", "Builtins"], "group"), _fGroup)
			, ((["ALGT", "Builtins"], "error"), _fError)
			, ((["ALGT", "Builtins"], "errorSugg"), _fError)

			]

_fParse		:: [ParseTree] -> Failable ParseTree
_fParse [pt]
	= fail "TODO: create parsing!" -- TODO


_fError		:: [ParseTree] -> Failable ParseTree
_fError [pt]
	= fail $ contents pt
_fError [ptMsg, ptErr]
	= failSugg (contents ptMsg, contents ptErr) 


_fGroup		:: [ParseTree] -> Failable ParseTree
_fGroup [pt]
	= return $ Literal (contents pt) (get ptLocation pt) (NoIndex ([], "String")) False


_intOp		:: (Int -> Int -> Int) -> [ParseTree] -> Failable ParseTree
_intOp op [i, (Int j _ _)]
		= i & over ptInt (`op` j) & return
_intOp _	args	= failSugg $ ("Wrong arguments for a builtin function expecting numbers, namely:\n" ++ (args |> toParsable & commas & indent)
				, "Use two integer arguments instead")

