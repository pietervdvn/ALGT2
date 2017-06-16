{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
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
	| FuncCall {_funcName :: FQName
		, _funcArgs :: [Expression a],  _expAnnot :: a}
	| Ascription {_ascExpr	:: Expression a
		, _ascType :: FQName, _expAnnot :: a}
	| SeqExp {_expSeq	:: [Expression a], _expAnnot :: a}
	deriving (Show, Eq, Functor)	
makeLenses ''Expression




instance ToString (Expression a) where
	toParsable (Var nm _)	= nm
	toParsable (DontCare _)	= "_"
	toParsable (ParseTree pt _)
				= toParsable pt
	toParsable (FuncCall ident args _)	
				= showFQ ident ++ inParens (args |> toParsable & commas)
	toParsable (Ascription exp as _)	
				= toParsable exp ++ ":" ++ showFQ as
	toParsable (SeqExp expr _)	
				= expr |> toParsable & unwords & inParens
	



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
		, cmb (:) expressionTerm expression'
		, expressionTerm |> (:[])]


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
		[(capture |> (:[])) <+> (lit "." **> capture)
		, capture |> (\nm -> ([], nm))]

