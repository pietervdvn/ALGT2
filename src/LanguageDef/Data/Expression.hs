{-# LANGUAGE TemplateHaskell, DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}
module LanguageDef.Data.Expression where

{- 
The metaExpressions contain the patterns and expressions that make up functions and natural deduction rules.

 -}

import Utils.All
import LanguageDef.Utils.LocationInfo
import LanguageDef.Data.ParseTree
import LanguageDef.Data.BNF (unescape)
import LanguageDef.Combiner


{- Expressions can be used both as pattern or as expression in a function/natural deduction rule, they are symmetrical. See Assets/MetaFunctionSyntax.language for more details

The data might carry extra information, such as _typing info_ or such as _meta info_

-}
data Expression a
	= Var	{ _varName 		:: Name
		, _expAnnot 		:: a
		, _expLocation		:: LocationInfo}
	| DontCare {_expAnnot 		:: a
		   , _expLocation	:: LocationInfo}
	| ParseTree {_expPT 		:: ParseTree'
		, _expAnnot		:: a
		, _expLocation		:: LocationInfo}
	| FuncCall {_expCallName	:: FQName
		, _expCallcArgs 	:: [Expression a]
		, _expAnnot 		:: a
		, _expLocation		:: LocationInfo}
	| Ascription {_ascExpr		:: Expression a
		, _ascType 		:: FQName
		, _expAnnot 		:: a
		, _expLocation		:: LocationInfo}
	| Split {_exp1 			:: Expression a
		, _exp2 		:: Expression a
		, _expAnnot 		:: a
		, _expLocation		:: LocationInfo}
	| SeqExp {_expSeq		:: [Expression a]
		 , _expAnnot 		:: a
		 , _expLocation		:: LocationInfo}
	deriving (Show, Eq, Functor)	
makeLenses ''Expression




instance ToString (Expression a) where
	toParsable (Var nm a _)	= nm
	toParsable (DontCare a _)
				= "_"
	toParsable (ParseTree pt a _)
				= toCoParsable pt
	toParsable (FuncCall ident args a _)	
				= showFQ ident ++ inParens (args |> toParsable & commas)
	toParsable (Ascription exp as a _)	
				= toParsable exp ++ ":" ++ showFQ as
	toParsable (SeqExp expr a _)	
				= (expr |> toParsable & unwords) & inParens

instance ToString' (a -> String) (Expression a) where
	toParsable' showA (Var nm a _)	= nm ++ showA a
	toParsable' showA (DontCare a _)
				= "_" ++ showA a
	toParsable' showA (ParseTree pt a _)
				= toCoParsable pt ++ showA a
	toParsable' showA (FuncCall ident args a _)	
				= showFQ ident ++ inParens (args |> toParsable & commas) ++ showA a
	toParsable' showA (Ascription exp as a _)	
				= toParsable exp ++ ":" ++ showFQ as ++ showA a
	toParsable' showA (SeqExp expr a _)	
				= (expr |> toParsable' showA & unwords ++ showA a) & inParens


	toCoParsable'	= toParsable'
	debug'		= toCoParsable'
	show' _		= toParsable


------------------------------- COMBINERS ------------------------------

choices' nm	= choices (["Functions"], nm)

{-
Converts the parsetree into an _untyped_ expression
-}
expressionTerm	:: Combiner (Expression ())
expressionTerm
	= choices' "expressionTerm"
		[ funcCall 
		, capture & withLocation' (flip Var ())
		, skip & withLocation' (const $ DontCare ())
		, int |> (\i -> Int i unknownLocation ()) & withLocation' (flip ParseTree ())
		, capture |> unescape |> (\str -> Literal str unknownLocation () False)
			& withLocation' (flip ParseTree ())
		, lit "(" **> expression <** lit ")"
		]


expression	:: Combiner (Expression ())
expression
	= expression' & withLocation' (flip SeqExp ()) |> (\seqExp -> case seqExp of
		SeqExp [exp] _ _	-> exp
		_			-> seqExp)

expression'	:: Combiner [Expression ()]
expression'
	= choices' "expression"
		[ascription	|> (:[])
		, splitExpression |> (:[])
		, cmb (:) expressionTerm expression'
		, expressionTerm |> (:[])]

splitExpression	:: Combiner (Expression ())
splitExpression
	= choices' "splitExpression"
		[(expressionTerm <+> (lit "&" **> expression))
			& withLocation (\li (exp1, exp2) -> Split exp1 exp2 () li)]


ascription	:: Combiner (Expression ())
ascription
	= choices' "ascription"
		[(expressionTerm <+> (lit ":" **> ident))
			& withLocation (\li (exp, typ) -> Ascription exp typ () li) ]


funcCall	:: Combiner (Expression ())
funcCall
	= choices' "funcCall"
		[(ident <+> (lit "(" **> arguments <** lit ")"))
			& withLocation (\li (id, args) -> FuncCall id args () li)
		]

arguments	:: Combiner [Expression ()]
arguments
	= choices' "arguments"
		[ cmb (:) expression (lit "," **> arguments)
		, expression |> (:[])
		]


ident		:: Combiner ([Name], Name)
ident	= choices' "ident"
		[ cmb (\head (tail, nm) -> (head:tail, nm))
			capture (lit "." **> ident)
		, capture |> (,) []]

