module LanguageDef.Syntax.All (module S
		, BNF.unescape, BNF.removeTail, BNF.BNF
		, Pt2Syntax.nls, Pt2Syntax.nl, Pt2Syntax.syntaxDecl'
		, asSyntaxUnchecked, asSyntaxUnchecked', asSyntax, asSyntax', _patchFullQualifies
		) where

import Utils.All

import Data.Map as M

import LanguageDef.Tools.ExceptionInfo
import LanguageDef.Syntax.BNF as BNF
import LanguageDef.Syntax.Syntax as S
import LanguageDef.Syntax.ParseTree as S
import LanguageDef.Syntax.MetaSyntax as S
import LanguageDef.Syntax.Pt2Syntax as Pt2Syntax
import LanguageDef.Syntax.Combiner as S


{- | Converts a BNF into a syntax
>>> toParsable bnfSyntax == asSyntax "Syntax" (toParsable bnfSyntax) & crash & toParsable
True
-}
asSyntaxUnchecked	:: Name -> String -> Failable Syntax
asSyntaxUnchecked syntaxName syntaxString
	= inMsg' ("Loading "++syntaxName++" as unchecked syntax") $
	  do	pt	<- parse ("asSyntaxUnchecked: "++show syntaxName) (M.singleton ["Syntax"] bnfSyntax, ["Syntax"]) "syntax" syntaxString
		let pt'	= removeHidden pt
		syntax	<- interpret syntaxDecl' pt'
		syntax & _patchFullQualifies [syntaxName] & return

-- Little hack: everything is fully qualified, including syntacticForm calls. When they are just parsed, they are not yet fully qualified and the exact resolution should still be done. For now, the bootstraps get manual patches for full qualifications
_patchFullQualifies	:: [Name] -> Syntax -> Syntax
_patchFullQualifies ns syntax
	= syntax |> over (syntChoices . mapped) (overRuleCall $ _patchBNFName ns)

_patchBNFName ns ([], name)	=  (ns, name)
_patchBNFName _ fqname		= fqname

asSyntaxUnchecked' nm str
	= asSyntaxUnchecked nm str & crash

asSyntax	:: Name -> String -> Failable Syntax
asSyntax 	= asSyntaxUnchecked



asSyntax' nm contents
	= asSyntax nm contents & crash
