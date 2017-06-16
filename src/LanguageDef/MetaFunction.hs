{-# LANGUAGE TemplateHaskell #-}
module LanguageDef.MetaFunction where


import Utils.All

import LanguageDef.MetaExpression
import LanguageDef.LocationInfo
import LanguageDef.Syntax.All

import Data.Maybe

{- A metafunction represents a function over parsetrees -}


data Function a	= Function
	{ _funcName	:: Name
	, _funcTypes	:: [(Maybe Name, Name)]
	, _funcClauses	:: [FunctionClause a]
	, _funcDoc	:: MetaInfo
	} deriving (Show, Eq)


data FunctionClause a = FunctionClause 
	{ _clausePatterns	:: [Expression a]
	, _clauseResult		:: Expression a
	, _clauseDoc		:: MetaInfo
	, _clauseFuncName	:: Name
	} deriving (Show, Eq)
makeLenses ''Function
makeLenses ''FunctionClause


functions	:: Combiner [Function LocationInfo]
functions = choices "functions"
		[ cmb (:) function functions
		, function |> (:[])
		]


function	:: Combiner (Function LocationInfo)
function = choices "function"
		[(nls <+> functionSignature <+> skip **> functionClauses)
			& withLocation (,)
			|> (\(li, (docs,((nm, types), clauses))) -> Function nm types clauses (MetaInfo li (unlines docs)) )
		]


functionSignature	:: Combiner (Name, [(Maybe Name, Name)])
functionSignature
	= choices "signature"
		[ capture <+> (lit ":" **> cmb (\tps tp -> tps ++ [tp]) functionTypes (skip **> ident))
		, capture <+> (lit ":" **> (ident |> (:[])))
		]


functionTypes	:: Combiner [(Maybe Name, Name)]
functionTypes
	= choices "types" 
		[ cmb (:) ident (skip **> functionTypes)
		, ident |> (:[])
		]


functionClauses	:: Combiner [FunctionClause LocationInfo]
functionClauses = choices "funcClauses"
			[ cmb (:) functionClause functionClauses
			, functionClause |> (:[])
			]

functionClause	:: Combiner (FunctionClause LocationInfo)
functionClause 	= choices "funcClause"
			[ (capture <+> lit "(" **> (arguments <+> lit ")" **> lit "=" **> expression <+> nl))
				& withLocation (,)
				|> _funcClause
			]

_funcClause	:: (LocationInfo, (String, ([Expression LocationInfo], (Expression LocationInfo, Maybe String))))
			-> FunctionClause LocationInfo
_funcClause (li, (nm, (pats, (result, comment))))
	= FunctionClause pats result (MetaInfo li $ fromMaybe "" comment) nm
		-- The name is kept for validation afterwards; the name should be the function name


instance ToString (FunctionClause a) where
	toParsable (FunctionClause pats result mi name)
		= name ++ inParens (pats |> toParsable & commas) ++"\t = " ++ toParsable result ++ toCoParsable mi


instance ToString (Function a) where
	toParsable (Function nm tps clauses mi)
	      = unlines $ 
		[ toParsable mi
		, nm ++ "\t : "++(init tps |> showIdent & intercalate " × ") ++ (if length tps > 1 then " → " else "") ++ (last tps & showIdent)
		]  
		++ clauses |> toParsable



