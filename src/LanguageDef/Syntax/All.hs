module LanguageDef.Syntax.All (module S
		, BNF.unescape, BNF.removeTail, BNF.BNF, BNF.FQName
		, asSyntaxUnchecked, asSyntaxUnchecked', asSyntax, asSyntax'
		) where

import Utils.All

import Data.Map as M

import LanguageDef.Syntax.BNF as BNF
import LanguageDef.Syntax.Syntax as S
import LanguageDef.Syntax.ParseTree as S
import LanguageDef.Syntax.MetaSyntax as S
import LanguageDef.Syntax.Pt2Syntax as S
import LanguageDef.Syntax.Combiner as S


{- | Converts a BNF into a syntax
>>> toParsable bnfSyntax == asSyntax "Syntax" (toParsable bnfSyntax) & either error toParsable 
True
-}
asSyntaxUnchecked	:: Name -> String -> Either String Syntax
asSyntaxUnchecked syntaxName syntaxString
	= inMsg ("While parsing the syntax with asSyntaxUnchecked") $
	  do	pt	<- parse ("Code: "++show syntaxName) (M.singleton ["Syntax"] bnfSyntax, ["Syntax"]) "syntax" syntaxString
		let pt'	= removeHidden pt
		syntax	<- interpret (syntaxDecl' [syntaxName]) pt'
		return syntax

asSyntaxUnchecked' nm str
	= asSyntaxUnchecked nm str & either error id

asSyntax	:: Name -> String -> Either String Syntax
asSyntax nm str
	= do 	syntax	<- asSyntaxUnchecked nm str
		check (asSyntaxes' [nm] syntax)
		return syntax



asSyntax' nm contents
	= asSyntax nm contents & either error id
