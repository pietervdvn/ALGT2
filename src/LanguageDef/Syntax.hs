{-# LANGUAGE MultiParamTypeClasses #-}
module LanguageDef.Syntax where

{- 
Gives the "syntax" language aspect of a language
 -}

import Utils.All

import LanguageDef.LanguageAspect
import LanguageDef.Syntax.Syntax
import LanguageDef.Syntax.ParseTree
import LanguageDef.Syntax.MetaSyntax
import LanguageDef.Syntax.Pt2Syntax


instance LanguageAspect () Syntax where
	name _ _	= "Syntax"
	author _ _	= "pietervdvn"
	version	_ _
		= [0,0,1]
	parsingSyntax _
			= (bnfSyntax, "syntax", syntaxDecl')
	



