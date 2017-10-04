module LanguageDef.Syntax.Pt2Syntax where

import Utils.All

import LanguageDef.Syntax.MetaSyntax (bnfSyntax)
import LanguageDef.Syntax.ParseTree
import LanguageDef.Syntax.Syntax (Syntax, createSyntax, SyntacticForm (SyntacticForm))
import LanguageDef.Syntax.BNF as BNF

import LanguageDef.Syntax.Combiner
import qualified LanguageDef.Syntax.Combiner as CMB
import LanguageDef.Utils.LocationInfo

import Data.Maybe


choices'	:: String -> [Combiner a] -> Combiner a
choices'	str
	= choices (["Syntax"], str)

------------------------- CONVERSION OF THE PARSETREE TO BNF --------------------------------



-- Comment line, without leading #
comment	:: Combiner String
comment	= choices' "comment" [cmb seq (lit "#") capture] 

-- A newline line, possibly containing a comment
nl	:: Combiner (Maybe String)
nl	= choices' "nl" 
		[ comment |> Just
		, lit "\n" |> const Nothing
		]

-- One or more newlines, possibly containing comments
nls	:: Combiner [String]
nls	= choices' "nls"
		[ cmb (\head tail -> maybe tail (:tail) head) nl nls
		, nl |> maybeToList
		]


builtinValue	:: Combiner BNF
builtinValue	= choices' "builtins"
			(knownBuiltins |> BNF.BuiltIn False |> Value)

bnfTerm	:: Combiner BNF
bnfTerm
	= choices' "bnfTerm"
		[ capture |> unescape |> BNF.Literal
		, cmb (curry BNF.RuleCall) (capture {-identifier-} |> (:[])) (cmb seq (lit ".") capture {-identifier-})
		, capture |> (\nm -> BNF.RuleCall ([], nm))
		, builtinValue
		, cmb seq (lit "$") (bnfTerm |> BNF.Group) 
		]


bnfSeq	:: Combiner BNF
bnfSeq	= choices' "bnfSeq"
		[ cmb (\h t -> BNF.Seq [h, t]) bnfTerm bnfSeq
		, bnfTerm ]

barC	:: Combiner (Maybe String)
barC	= choices' "bar"
		[ lit "|" |> const Nothing
		, nl <** lit "\t" <** capture{-ws-} <** lit "|"
		]


bnfchoices'	:: Combiner [(BNF, MetaInfo)]
bnfchoices'
	= choices' "bnfChoices"
		[ cmb (\bnfTerm (comment, tail) -> (bnfTerm, comment):tail)
			bnfSeq (cmb (,) (barC |> fromMaybe "" & withLocation MetaInfo) bnfchoices')
		, cmb (,) bnfSeq (nl |> fromMaybe "" & withLocation MetaInfo) |> (:[])
		]


assign	:: Combiner (BNF -> BNF)
assign	= choices' "assign"
		[lit "::=" |> const (injectWS . normalize)
		, lit "~~=" |> const id]

bnfDecl	:: Combiner SyntacticForm
bnfDecl
	= choices' "bnfDecl"
		[ cmb (,) (nls |> concat & withLocation MetaInfo)
			(cmb (,) capture {-Identifier: name-}
			(cmb (over (mapped . _1)) assign 
				bnfchoices'))
		, cmb (,) capture {-Identifier:Name-} (cmb (over (mapped . _1)) assign bnfchoices')
			& withLocation (\li decl -> (MetaInfo li "", decl) )
		] |> (\(mi, (nm, choices')) -> SyntacticForm nm (choices' |> fst) (choices' |> snd) mi)


syntaxDecl	:: Combiner [SyntacticForm]
syntaxDecl
	= choices' "syntax"
		[ cmb (:) bnfDecl syntaxDecl
		, bnfDecl |> (:[])
		]


syntaxDecl'	:: Combiner Syntax
syntaxDecl'
	= syntaxDecl |> createSyntax


