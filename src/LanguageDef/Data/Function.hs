{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module LanguageDef.Data.Function where


import Utils.All

import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.Checkable
import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.Grouper

import LanguageDef.Data.Expression hiding (choices')
import LanguageDef.Data.SyntFormIndex hiding (choices')

import LanguageDef.Combiner
import LanguageDef.MetaSyntax (nls, nl, ident, typeIdent)

import Data.Maybe
import Data.Map as M
import Data.List as L

import Control.Monad

{- A metafunction represents a function over parsetrees -}
data Function' a	= Function
	{ _funcName		:: Name
	, _funcArgTypes		:: [FQName]
	, _funcRetType		:: FQName
	, _funcClauses		:: [FunctionClause' a]
	, _funcDoc		:: MetaInfo
	} deriving (Show, Eq, Functor)


type Function	= Function' SyntFormIndex


data FunctionClause' a = FunctionClause 
	{ _clausePatterns	:: [Expression' a]
	, _clauseResult		:: Expression' a
	, _clauseDoc		:: MetaInfo
	, _clauseFuncName	:: Name
	} deriving (Show, Eq, Functor)


makeLenses ''Function'
makeLenses ''FunctionClause'

type FunctionClause	= FunctionClause' SyntFormIndex


instance Checkable (Function' a) where
	check (Function name argTps retTp clauses docs)
		= inMsg' ("While checking function "++show name) $
		  	assert' (clauses |> get clauseFuncName & all (== name)) $
				"Some clauses have a different name. The function name is "++show name++
				", but a clause is named "++(clauses |> get clauseFuncName & sort & nub & commas)





choices' nm	= choices (["Functions"], nm) 


_functionsCmb	:: Combiner [Function' ()]
_functionsCmb = choices' "functions"
		[ cmb (:) function _functionsCmb
		, function |> (:[])
		]

functionsCmb	:: Combiner (Grouper (Function' ()))
functionsCmb	= _functionsCmb ||>> (|> const ())
			|> asGrouper ("function", "functions") (get funcName)



function	:: Combiner (Function' ())
function = choices' "function"
		[(nls <+> functionSignature <+> skip **> functionClauses)
			& withLocation (,)
			|> (\(li, (docs,((nm, types), clauses))) -> Function nm (init types) (last types) clauses (MetaInfo li (unlines docs)) )
		]


functionSignature	:: Combiner (Name, [FQName])
functionSignature
	= choices' "signature"
		[ capture <+> (lit ":" **> cmb (\tps tp -> tps ++ [tp]) functionTypes (skip **> typeIdent))
		, capture <+> (lit ":" **> (typeIdent |> (:[])))
		]


functionTypes	:: Combiner [FQName]
functionTypes
	= choices' "types" 
		[ cmb (:) typeIdent (skip **> functionTypes)
		, typeIdent |> (:[])
		]


functionClauses	:: Combiner [FunctionClause' ()]
functionClauses = choices' "funcClauses"
			[ cmb (:) functionClause functionClauses
			, functionClause |> (:[])
			]

functionClause	:: Combiner (FunctionClause' ())
functionClause 	= choices' "funcClause"
			[ (capture <+> lit "(" **> (arguments <+> lit ")" **> skip **> expression <+> nl))
				& withLocation (,)
				|> _funcClause
			]

_funcClause	:: (LocationInfo, (String, ([Expression' a], (Expression' a, Maybe String))))
			-> FunctionClause' a
_funcClause (li, (nm, (pats, (result, comment))))
	= FunctionClause pats result (MetaInfo li $ fromMaybe "" comment) nm
		-- The name is kept for validation afterwards; the name should be the function name


instance ToString (FunctionClause' a) where
	toParsable (FunctionClause pats result mi name)
		= name ++ inParens (pats |> toParsable & commas) ++"\t = " ++ toParsable result ++ toCoParsable mi


instance ToString (Function' a) where
	toParsable (Function nm argTps retTp clauses mi)
	      = unlines $ 
		[ toParsable mi
		, nm ++ "\t : "++(argTps |> showFQ & over _last (++" → ")  & intercalate " × ") ++ showFQ retTp
		]  
		++ clauses |> toParsable


instance Infoable (Function' a) where
	getInfo f
		= AllInfo (get funcName f) "Function" (get funcDoc f) (toParsable f)
