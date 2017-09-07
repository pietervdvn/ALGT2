{-# LANGUAGE TemplateHaskell, DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}
module LanguageDef.MetaExpression where

{- 
The metaExpressions contain the patterns and expressions that make up functions and natural deduction rules.

 -}

import Utils.All
import LanguageDef.Syntax.All
import LanguageDef.LocationInfo


{- Expressions can be used both as pattern or as expression in a function/natural deduction rule, they are symmetrical. See Assets/MetaFunctionSyntax.language for more details

The data might carry extra information, such as _typing info_ or such as _meta info_

-}
data Expression a
	= Var {_varName :: Name, 		_expAnnot :: a}
	| DontCare {				_expAnnot :: a}
	| ParseTree {_expPT :: ParseTree', 	_expAnnot :: a}
	| FuncCall {_expCallName :: FQName
		, _expCallcArgs :: [Expression a],  _expAnnot :: a}
	| Ascription {_ascExpr	:: Expression a
		, _ascType :: FQName, _expAnnot :: a}
	| Split {_exp1 :: Expression a, _exp2 :: Expression a, _expAnnot :: a}
	| SeqExp {_expSeq	:: [Expression a], _expAnnot :: a}
	deriving (Show, Eq, Functor)	
makeLenses ''Expression




instance ToString (Expression a) where
	toParsable (Var nm a)	= nm
	toParsable (DontCare a)	= "_"
	toParsable (ParseTree pt a)
				= toCoParsable pt
	toParsable (FuncCall ident args a)	
				= showFQ ident ++ inParens (args |> toParsable & commas)
	toParsable (Ascription exp as a)	
				= toParsable exp ++ ":" ++ showFQ as
	toParsable (SeqExp expr a)	
				= (expr |> toParsable & unwords) & inParens

instance ToString' (a -> String) (Expression a) where
	toParsable' showA (Var nm a)	= nm ++ showA a
	toParsable' showA (DontCare a)	= "_" ++ showA a
	toParsable' showA (ParseTree pt a)
				= toCoParsable pt ++ showA a
	toParsable' showA (FuncCall ident args a)	
				= showFQ ident ++ inParens (args |> toParsable & commas) ++ showA a
	toParsable' showA (Ascription exp as a)	
				= toParsable exp ++ ":" ++ showFQ as ++ showA a
	toParsable' showA (SeqExp expr a)	
				= (expr |> toParsable' showA & unwords ++ showA a) & inParens


	toCoParsable'	= toParsable'
	debug'		= toCoParsable'
	show' _		= toParsable

------------------------------- COMBINERS ------------------------------

choices' nm	= choices (["Functions"], nm)

{-
Converts the parsetree into an _untyped_ expression
-}
expressionTerm	:: Combiner (Expression LocationInfo)
expressionTerm
	= choices' "expressionTerm"
		[ funcCall 
		, capture & withLocation' Var
		, skip & withLocation' (const DontCare)
		, int |> (\i -> Int i unknownLocation ()) & withLocation' ParseTree
		, capture |> unescape |> (\str -> Literal str unknownLocation () False)
			& withLocation' ParseTree
		, lit "(" **> expression <** lit ")"
		]


expression	:: Combiner (Expression LocationInfo)
expression
	= expression' & withLocation' SeqExp |> (\seqExp -> case seqExp of
		SeqExp [exp] _	-> exp
		_		-> seqExp)

expression'	:: Combiner [Expression LocationInfo]
expression'
	= choices' "expression"
		[ascription	|> (:[])
		, splitExpression |> (:[])
		, cmb (:) expressionTerm expression'
		, expressionTerm |> (:[])]

splitExpression	:: Combiner (Expression LocationInfo)
splitExpression
	= choices' "splitExpression"
		[(expressionTerm <+> (lit "&" **> expression))
			& withLocation (\li (exp1, exp2) -> Split exp1 exp2 li)]


ascription	:: Combiner (Expression LocationInfo)
ascription
	= choices' "ascription"
		[(expressionTerm <+> (lit ":" **> ident))
			& withLocation (\li (exp, typ) -> Ascription exp typ li) ]


funcCall	:: Combiner (Expression LocationInfo)
funcCall
	= choices' "funcCall"
		[(ident <+> (lit "(" **> arguments <** lit ")"))
			& withLocation (\li (id, args) -> FuncCall id args li)
		]

arguments	:: Combiner [Expression LocationInfo]
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

