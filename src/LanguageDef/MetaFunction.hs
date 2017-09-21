{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module LanguageDef.MetaFunction where


import Utils.All

import LanguageDef.MetaExpression hiding (choices')
import LanguageDef.LocationInfo
import LanguageDef.Syntax.All
import LanguageDef.Grouper

import Data.Maybe
import Data.Map as M
import Data.List as L

import Control.Monad

{- A metafunction represents a function over parsetrees -}
data Function' a	= Function
	{ _funcName	:: Name
	, _funcArgTypes	:: [FQName]
	, _funcRetType	:: FQName
	, _funcClauses	:: [FunctionClause a]
	, _funcDoc	:: MetaInfo
	} deriving (Show, Eq, Functor)


type Function	= Function' SyntFormIndex


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


makeLenses ''Function'
makeLenses ''FunctionClause


instance Checkable (Function' a) where
	check (Function name argTps retTp clauses docs)
		= inMsg ("While checking function "++show name) $
		  	unless (clauses |> get clauseFuncName & all (== name)) $ Left $
				"Some clauses have a different name. The function name is "++show name++
				", but a clause is named "++(clauses |> get clauseFuncName & sort & nub & commas)





choices' nm	= choices (["Functions"], nm) 


_functionsCmb	:: Combiner [Function' LocationInfo]
_functionsCmb = choices' "functions"
		[ cmb (:) function _functionsCmb
		, function |> (:[])
		]

functionsCmb	:: Combiner (Grouper (Function' ()))
functionsCmb	= _functionsCmb ||>> (|> const ())
			|> asGrouper ("function", "functions") (get funcName)



function	:: Combiner (Function' LocationInfo)
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


instance ToString (Function' a) where
	toParsable (Function nm argTps retTp clauses mi)
	      = unlines $ 
		[ toParsable mi
		, nm ++ "\t : "++(argTps |> showFQ & over _last (++" → ")  & intercalate " × ") ++ showFQ retTp
		]  
		++ clauses |> toParsable

