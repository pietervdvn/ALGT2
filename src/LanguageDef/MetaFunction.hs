{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module LanguageDef.MetaFunction where


import Utils.All

import LanguageDef.MetaExpression hiding (choices')
import LanguageDef.LocationInfo
import LanguageDef.Syntax.All

import Data.Maybe
import Data.Map as M
import Data.List as L

{- A metafunction represents a function over parsetrees -}
data Function a	= Function
	{ _funcName	:: Name
	, _funcArgTypes	:: [FQName]
	, _funcRetType	:: FQName
	, _funcClauses	:: [FunctionClause a]
	, _funcDoc	:: MetaInfo
	} deriving (Show, Eq, Functor)


data FunctionClause a = FunctionClause 
	{ _clausePatterns	:: [Expression a]
	, _clauseResult		:: Expression a
	, _clauseDoc		:: MetaInfo
	, _clauseFuncName	:: Name
	} deriving (Show, Eq, Functor)


type SyntForm	= FQName
data SyntFormIndex = SyntFormIndex
			{ _syntForm	:: SyntForm
			, _syntChoice	:: Int	-- To what choice does it correspond?
			, _syntSeqInd	:: Maybe Int	-- To what index in the sequence of the choice does it correspond?
			} 
			| NoIndex SyntForm
			deriving (Show, Eq)

removeIndex'	:: SyntFormIndex -> SyntFormIndex
removeIndex' (SyntFormIndex sf _ _)
	= NoIndex sf
removeIndex' noIndex	= noIndex

removeIndex i	= i & removeIndex' & (\(NoIndex sf) -> sf)

instance ToString SyntFormIndex where
	toParsable (NoIndex sf)	= showFQ sf
	toParsable (SyntFormIndex f ch ind)
				= showFQ f ++":"++show ch++(ind |> show |> ("."++) & fromMaybe "")


type TypedFunction	= Function SyntFormIndex

data Functions' a	= Functions 
		{ _functions :: Map Name (Function a)
		, _functionOrder :: [Name]}
	deriving (Show, Eq)

type Functions	= Functions' SyntFormIndex
-- TODO Check that each function clause bears the same name as the function itself


makeLenses ''Functions'
makeLenses ''Function
makeLenses ''FunctionClause

choices' nm	= choices (["Functions"], nm) 


functionsCmb	:: Combiner [Function LocationInfo]
functionsCmb = choices' "functions"
		[ cmb (:) function functionsCmb
		, function |> (:[])
		]


function	:: Combiner (Function LocationInfo)
function = choices' "function"
		[(nls <+> functionSignature <+> skip **> functionClauses)
			& withLocation (,)
			|> (\(li, (docs,((nm, types), clauses))) -> Function nm (init types) (last types) clauses (MetaInfo li (unlines docs)) )
		]


functionSignature	:: Combiner (Name, [FQName])
functionSignature
	= choices' "signature"
		[ capture <+> (lit ":" **> cmb (\tps tp -> tps ++ [tp]) functionTypes (skip **> ident))
		, capture <+> (lit ":" **> (ident |> (:[])))
		]


functionTypes	:: Combiner [FQName]
functionTypes
	= choices' "types" 
		[ cmb (:) ident (skip **> functionTypes)
		, ident |> (:[])
		]


functionClauses	:: Combiner [FunctionClause LocationInfo]
functionClauses = choices' "funcClauses"
			[ cmb (:) functionClause functionClauses
			, functionClause |> (:[])
			]

functionClause	:: Combiner (FunctionClause LocationInfo)
functionClause 	= choices' "funcClause"
			[ (capture <+> lit "(" **> (arguments <+> lit ")" **> skip **> expression <+> nl))
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
	toParsable (Function nm argTps retTp clauses mi)
	      = unlines $ 
		[ toParsable mi
		, nm ++ "\t : "++(argTps |> showFQ & over _last (++" → ")  & intercalate " × ") ++ showFQ retTp
		]  
		++ clauses |> toParsable

instance ToString (Functions' a) where
	toParsable (Functions funcs order)
		= let	order'	= order ++ (funcs & M.keys & L.filter (`M.notMember` funcs)) in
			order'	|> (funcs ! )
				|> toParsable
				& unlines
