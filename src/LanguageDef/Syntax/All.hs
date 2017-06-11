module LanguageDef.Syntax.All (module S, Syntax.Syntax, removeTail, syntax, syntaxChoices, asSyntaxes,  asSyntax, asSyntax', mergeSyntax, mergeSyntax', unescape, asSyntaxUnchecked, asSyntaxUnchecked', asSyntaxes') where

import Utils.All

import LanguageDef.Syntax.Syntax as Syntax
import LanguageDef.Syntax.ParseTree as S
import LanguageDef.Syntax.MetaSyntax as S
import LanguageDef.Syntax.Pt2Syntax as S
import LanguageDef.Syntax.Combiner as S


{- | Converts a BNF into a syntax
>>> toParsable bnfSyntax == asSyntax "bnf" (toParsable bnfSyntax) & either error toParsable 
True
-}
asSyntaxUnchecked	:: Name -> String -> Either String Syntax
asSyntaxUnchecked syntaxName syntaxString
	= do	pt	<- parse ("Code: "++show syntaxName) (asSyntaxes bnfSyntax) "syntax" syntaxString
		let pt'	= removeHidden pt
		syntax	<- interpret syntaxDecl' pt'
		return syntax

asSyntaxUnchecked' nm str
	= asSyntaxUnchecked nm str & either error id

asSyntax	:: Name -> String -> Either String Syntax
asSyntax nm str
	= do 	syntax	<- asSyntaxUnchecked nm str
		check (asSyntaxes' syntax)
		return syntax



asSyntax' nm contents
	= asSyntax nm contents & either error id
