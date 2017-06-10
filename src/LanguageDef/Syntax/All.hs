module LanguageDef.Syntax.All (module S, Syntax.Syntax, Syntax.asSyntaxes,  asSyntax, asSyntax', mergeSyntax, mergeSyntax', unescape) where

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
asSyntax	:: Name -> String -> Either String Syntax
asSyntax syntaxName syntaxString
	= do	pt	<- parse ("Code: "++show syntaxName) (asSyntaxes bnfSyntax) "syntax" syntaxString
		let pt'	= removeHidden pt
		syntax	<- interpret syntaxDecl' pt'
		return syntax

asSyntax' nm contents
	= asSyntax nm contents & either error id
