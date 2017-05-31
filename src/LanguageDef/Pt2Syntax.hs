module LanguageDef.Pt2Syntax where

import Utils.All

import LanguageDef.MetaSyntax (bnfSyntax)
import LanguageDef.ParseTree
import LanguageDef.Syntax (Syntax)
import LanguageDef.Syntax as BNF

import Data.Maybe


t' x r	= parse "TEST" bnfSyntax r x 
		& either error id
		& removeHidden
		
t x r c	= t' x r 
		& interpret c
		& either error id

ps	= parse "TEST" bnfSyntax "syntax" (toParsable bnfSyntax)
		& either error id
		& removeHidden
		& interpret syntaxDecl'
		& either error id




------------------------- CONVERSION OF THE PARSETREE TO BNF --------------------------------

convert		:: ParseTree' -> Syntax
convert pt	= todo



-- Comment line, without leading #
comment	:: Combiner String
comment	= choices "comment" [cmb seq (lit "#") capture] 

-- A newline line, possibly containing a comment
nl	:: Combiner (Maybe String)
nl	= choices "nl" 
		[ comment |> Just
		, lit "\n" |> const Nothing
		]

-- One or more newlines, possibly containing comments
nls	:: Combiner [String]
nls	= choices "nls"
		[ cmb (\head tail -> maybe tail (:tail) head) nl nls
		, nl |> maybe [] (:[])
		]


builtinValue	:: Combiner BNF
builtinValue	= choices "builtins"
			(knownBuiltins |> BNF.BuiltIn False |> Value)

bnfTerm	:: Combiner BNF
bnfTerm	= choices "bnfTerm"
		[ capture |> unescape |> BNF.Literal
		, capture |> BNF.RuleCall
		, builtinValue
		, cmb seq (lit "$") (bnfTerm |> BNF.Group) 
		]


bnfSeq	:: Combiner BNF
bnfSeq	= choices "bnfSeq"
		[ cmb (\h t -> BNF.Seq [h, t]) bnfTerm bnfSeq
		, bnfTerm ]

barC	:: Combiner (Maybe String)
barC	= choices "bar"
		[ lit "|" |> const Nothing
		, nl <** (lit "\t") <** (capture {-ws-}) <** (lit "|")
		]


bnfChoices	:: Combiner [(BNF, MetaInfo)]
bnfChoices 	
	= choices "bnfChoices"
		[ cmb (\bnfTerm (comment, tail) -> (bnfTerm, comment):tail)
			bnfSeq (cmb (,) (barC |> fromMaybe "" & withLocation MetaInfo) bnfChoices)
		, cmb (,) bnfSeq (nl |> fromMaybe "" & withLocation MetaInfo) |> (:[])
		]


assign	:: Combiner (BNF -> BNF)
assign	= choices "assign"
		[lit "::=" |> const injectWS
		, lit "~~=" |> const id]

bnfDecl	:: Combiner (Name, ([(BNF, MetaInfo)], MetaInfo))
bnfDecl	= choices "bnfDecl"
		[ cmb (,) (nls |> concat & withLocation MetaInfo) 
			(cmb (,) capture {-Identiier: name-}
			(cmb (over (mapped . _1)) assign 
				bnfChoices))
		] |> (\(mi, (nm, choices)) -> (nm, (choices, mi)))


syntaxDecl	:: Combiner [(Name, ([(BNF, MetaInfo)], MetaInfo))]
syntaxDecl = choices "syntax"
		[ cmb (:) bnfDecl syntaxDecl
		, bnfDecl |> (:[])
		]

syntaxDecl'	:: Combiner Syntax
syntaxDecl'
	= syntaxDecl |> createSyntax


