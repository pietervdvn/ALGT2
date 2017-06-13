module LanguageDef.Syntax.Pt2Syntax where

import Utils.All

import LanguageDef.Syntax.MetaSyntax (bnfSyntax)
import LanguageDef.Syntax.ParseTree
import LanguageDef.Syntax.Syntax (Syntax, createSyntax)
import LanguageDef.Syntax.BNF as BNF
import LanguageDef.Syntax.Combiner
import LanguageDef.LocationInfo

import Data.Maybe




------------------------- CONVERSION OF THE PARSETREE TO BNF --------------------------------



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

bnfTerm	:: [Name] -> Combiner BNF
bnfTerm ns
	= choices "bnfTerm"
		[ capture |> unescape |> BNF.Literal
		, cmb (\ns nm -> BNF.RuleCall (ns, nm)) (capture {-identifier-} |> (:[])) (cmb seq (lit ".") (capture {-identifier-}))
		, capture |> (\nm -> BNF.RuleCall (ns, nm))
		, builtinValue
		, cmb seq (lit "$") (bnfTerm ns |> BNF.Group) 
		]


bnfSeq	:: [Name] -> Combiner BNF
bnfSeq ns
	= choices "bnfSeq"
		[ cmb (\h t -> BNF.Seq [h, t]) (bnfTerm ns) (bnfSeq ns)
		, bnfTerm ns ]

barC	:: Combiner (Maybe String)
barC	= choices "bar"
		[ lit "|" |> const Nothing
		, nl <** (lit "\t") <** (capture {-ws-}) <** (lit "|")
		]


bnfChoices	:: [Name] -> Combiner [(BNF, MetaInfo)]
bnfChoices ns	
	= choices "bnfChoices"
		[ cmb (\bnfTerm (comment, tail) -> (bnfTerm, comment):tail)
			(bnfSeq ns) (cmb (,) (barC |> fromMaybe "" & withLocation MetaInfo) (bnfChoices ns))
		, cmb (,) (bnfSeq ns) (nl |> fromMaybe "" & withLocation MetaInfo) |> (:[])
		]


assign	:: Combiner (BNF -> BNF)
assign	= choices "assign"
		[lit "::=" |> const (injectWS . normalize)
		, lit "~~=" |> const id]

bnfDecl	:: [Name] -> Combiner (Name, ([(BNF, MetaInfo)], MetaInfo))
bnfDecl ns
	= choices "bnfDecl"
		[ cmb (,) (nls |> concat & withLocation MetaInfo) 
			(cmb (,) capture {-Identifier: name-}
			(cmb (over (mapped . _1)) assign 
				(bnfChoices ns)))
		, (cmb (,) capture {-Identifier:Name-} (cmb (over (mapped . _1)) assign (bnfChoices ns)))
			& withLocation (\li decl -> (MetaInfo li "", decl) )
		] |> (\(mi, (nm, choices)) -> (nm, (choices, mi)))


syntaxDecl	:: [Name] -> Combiner [(Name, ([(BNF, MetaInfo)], MetaInfo))]
syntaxDecl ns 
	= choices "syntax"
		[ cmb (:) (bnfDecl ns) (syntaxDecl ns)
		, (bnfDecl ns) |> (:[])
		]


syntaxDecl'	:: [Name] -> Combiner Syntax
syntaxDecl' ns
	= syntaxDecl ns |> createSyntax


