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

import Debug.Trace

syntaxExtras	= [ (typeTop, _topSF)
		  , (typeBottom, _bottomSF)
		  ]


biLocations	= LocationInfo 0 0 0 0 "ALGT/Builtins.language"

_bottomSF	:: SyntacticForm
_bottomSF
	= SyntacticForm "⊥" [] [] $ MetaInfo biLocations
		"The bottom type (⊥) represents values which can not exist (or the intersection of all types). For example, a function which always returns an error; or gets stuck in an infinite loop, is said to have type \"bottom\". An intersection between two types (e.g. \"bool\" ∩ \"int\") does not have a result, and is represented with type bottom"

_topSF		:: SyntacticForm
_topSF	= SyntacticForm "⊤" [] [] $ MetaInfo biLocations
		"The top type (⊤) represents all possible values (or: the union of all types). For example, a function which accepts any value, will be declared as having ⊤ as input argument"
		



isBuiltinFunction	:: LanguageDef' ResolvedImport fr -> Function' x -> Bool
isBuiltinFunction ld f
 	= let msg	= "Testing "++ (get (langLocation . miFile) ld) in
		trace msg "ALGT.Builtins" == (get (langLocation . miFile) ld)


functions	:: Map FQName ((String -> String -> Failable ParseTree) -> [ParseTree] -> Failable ParseTree) 
functions 	= M.fromList
			[ ((["ALGT", "Builtins"], "plus"), _intOp (+))
			, ((["ALGT", "Builtins"], "min"), _intOp (-))
			, ((["ALGT", "Builtins"], "mul"), _intOp (*))
			, ((["ALGT", "Builtins"], "div"), _intOp div)
			, ((["ALGT", "Builtins"], "mod"), _intOp mod)
			
			, ((["ALGT", "Builtins"], "parse"), _fParse)
			, ((["ALGT", "Builtins"], "group"), _fGroup)
			, ((["ALGT", "Builtins"], "error"), _fError)
			, ((["ALGT", "Builtins"], "errorSugg"), _fError)
			, ((["ALGT", "Builtins"], "dynType"), _fId)

			]

_fId		:: a -> [ParseTree] -> Failable ParseTree
_fId _ [pt]	= return pt

_fParse		:: (String -> String -> Failable ParseTree) -> [ParseTree] -> Failable ParseTree
_fParse parseString [ptArg, ptParseAs]
	= do	let toParse	= contents ptArg
		let typeAs	= contents ptParseAs
		parseString toParse typeAs |> removeHidden |> removeRuleEnters


_fError		:: a -> [ParseTree] -> Failable ParseTree
_fError _ [pt]
	= fail $ contents pt
_fError _ [ptMsg, ptErr]
	= failSugg (contents ptMsg, contents ptErr) 


_fGroup		:: a -> [ParseTree] -> Failable ParseTree
_fGroup _ [pt]
	= return $ Literal (contents pt) (get ptLocation pt) (NoIndex ([], "String")) False


_intOp		:: (Int -> Int -> Int) -> a -> [ParseTree] -> Failable ParseTree
_intOp op _ [i, Int j _ _]
		= i & over ptInt (`op` j) & return
_intOp _ _	args	= failSugg ("Wrong arguments for a builtin function expecting numbers, namely:\n" ++ (args |> toParsable & commas & indent)
				, "Use two integer arguments instead")

