{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module LanguageDef.Data.Function where


import Utils.All

import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.Checkable
import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.Grouper

import LanguageDef.Data.Expression hiding (choices')

import LanguageDef.Combiner
import LanguageDef.MetaSyntax (nls, nl)

import Data.Maybe
import Data.Map as M
import Data.List as L

import Control.Monad

{- A metafunction represents a function over parsetrees -}
data Function' a	= Function
	{ _funcName		:: Name
	, _funcArgTypes		:: [FQName]
	, _funcRetType		:: FQName
	, _funcClauses		:: [FunctionClause a]
	, _funcDoc		:: MetaInfo
	} deriving (Show, Eq, Functor)


type Function	= Function' SyntFormIndex


data FunctionClause a = FunctionClause 
	{ _clausePatterns	:: [Expression a]
	, _clauseResult		:: Expression a
	, _clauseDoc		:: MetaInfo
	, _clauseFuncName	:: Name
	} deriving (Show, Eq, Functor)


-- TODO Move this to a more appropriate module
type SyntForm	= FQName
data SyntFormIndex = SyntFormIndex
			{ _syntIndForm	:: SyntForm
			, _syntIndChoice	:: Int	-- To what choice does it correspond?
			, _syntIndSeqInd	:: Maybe Int	-- To what index in the sequence of the choice does it correspond?
			} 
			| NoIndex { _syntIndForm	:: SyntForm}
			deriving (Show, Eq)

makeLenses ''SyntFormIndex


removeIndex'	:: SyntFormIndex -> SyntFormIndex
removeIndex' (SyntFormIndex sf _ _)
	= NoIndex sf
removeIndex' noIndex	= noIndex

removeIndex i	= i & removeIndex' & (\(NoIndex sf) -> sf)

-- Selects all elements in the list, for which the syntactic form is a smallest type
selectSmallest	:: (FQName -> FQName -> Bool) ->  [(a, SyntFormIndex)] -> [(a, SyntFormIndex)]
selectSmallest isSubtypeOf
	= _selectSmallest isSubtypeOf []

_selectSmallest	:: (FQName -> FQName -> Bool) -> [(a, SyntFormIndex)] -> [(a, SyntFormIndex)] -> [(a, SyntFormIndex)]
_selectSmallest isSubtypeOf [] (a:as)
		= _selectSmallest isSubtypeOf [a] as
_selectSmallest _ smallest []	= smallest
_selectSmallest isSubtypeOf smallest (a@(_, toJudge):as)
 -- if to judge is a subtype of a value from smallest:
 | any (isSubtypeOf (get syntIndForm toJudge)) (smallest |> snd |> get syntIndForm)
	= let	toJudgeFQN	= toJudge & get syntIndForm
		smallest'	= L.filter (not . isSubtypeOf toJudgeFQN . get syntIndForm . snd) smallest
		in
		_selectSmallest isSubtypeOf (a:smallest') as
 | otherwise
	= _selectSmallest isSubtypeOf smallest as


instance ToString SyntFormIndex where
	toParsable (NoIndex sf)	= showFQ sf
	toParsable (SyntFormIndex f ch ind)
				= showFQ f ++":"++show ch++(ind |> show |> ("."++) & fromMaybe "")


makeLenses ''Function'
makeLenses ''FunctionClause


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
		[ capture <+> (lit ":" **> cmb (\tps tp -> tps ++ [tp]) functionTypes (skip **> ident))
		, capture <+> (lit ":" **> (ident |> (:[])))
		]


functionTypes	:: Combiner [FQName]
functionTypes
	= choices' "types" 
		[ cmb (:) ident (skip **> functionTypes)
		, ident |> (:[])
		]


functionClauses	:: Combiner [FunctionClause ()]
functionClauses = choices' "funcClauses"
			[ cmb (:) functionClause functionClauses
			, functionClause |> (:[])
			]

functionClause	:: Combiner (FunctionClause ())
functionClause 	= choices' "funcClause"
			[ (capture <+> lit "(" **> (arguments <+> lit ")" **> skip **> expression <+> nl))
				& withLocation (,)
				|> _funcClause
			]

_funcClause	:: (LocationInfo, (String, ([Expression a], (Expression a, Maybe String))))
			-> FunctionClause a
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


instance Infoable (Function' a) where
	getInfo f
		= AllInfo (get funcName f) "Function" (get funcDoc f) (toParsable f)
