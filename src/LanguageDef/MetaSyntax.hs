module LanguageDef.MetaSyntax where

{- 

Defines the BNF of BNF, used to parse BNF

 -}

import Utils.All

import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.ExceptionInfo

import LanguageDef.Data.SyntacticForm
import LanguageDef.Data.BNF as BNF
import LanguageDef.Data.ParseTree (ParseTree, parse, removeHidden)

import LanguageDef.Combiner

import qualified Data.Map as M
import Data.Maybe (maybeToList)




----------- METASYNTAX: FORMAT OF A SYNTAX IN BNF ITSELF --------


{-

Parses a syntax; used for bootstrapping.
Note that syntaxcalls are _not_ fully qualified

-}
parseSyntax	:: Name -> String -> Failable Syntax
parseSyntax syntaxName syntaxString
	= inMsg' ("Loading "++syntaxName++" as unchecked syntax") $
	  do	pt	<- parse ("asSyntaxUnchecked: "++show syntaxName) (M.singleton ["Syntax"] bnfSyntax, ["Syntax"]) "syntax" syntaxString
		let pt'	= removeHidden pt
		interpret syntaxDecl' pt' |> patchNames [syntaxName]




-- Little hack: everything is fully qualified, including syntacticForm calls. When they are just parsed, they are not yet fully qualified yet and the exact resolution should still be done. For the metasyntaxes, this ful qualification is done manually
patchNames	:: [Name] -> Syntax -> Syntax
patchNames ns syntax
	= syntax |> over (syntChoices . mapped) (BNF.overRuleCall $ _patchBNFName ns)

_patchBNFName ns ([], name)	=  (ns, name)
_patchBNFName _ fqname		= fqname






helperSyntax	:: Syntax
helperSyntax	= createSyntax helperSyntax'

-- Syntax for newlines and comments
helperSyntax'	= [ syntForm "commentContents" "The contents of comments, such as what you are reading right now"
			[ choice "Body of the comment" [bi lineChar, call "commentContents"]
			, choice "Closing of the comment" [Literal "\n"]
			]
		, syntForm "comment" "A comment"
			[ choice "" [Literal "#", Group ( call "commentContents")]]
		, syntForm "nl" "A newline, possibly with a single comment"
			[ choice "" [call "comment"]
			, choice "" [Literal "\n"]
			]
		, syntForm "nls" "Multiple nls, possibly with comments"
			[ choice "" [call "nl", call "nls"]
			, choice "" [call "nl"]
			]
		]


bnfSyntax	:: Syntax
bnfSyntax	
	= createSyntax $ helperSyntax' ++
		[ syntForm "builtin" "The names of syntactic forms that are available as builtins"
			(knownBuiltins |> (\bisf -> choice (get biDocs bisf++"; "++get biRegex bisf) [Literal $ get biName bisf] ))
		, syntForm "bnfTerm" "A single term of BNF, thus either a literal, syntactic form call or builtin"
			[ choice "Literal value" [bi string]
			, choice "Syntactic form call in some namespace" [bi identifierUpper, Literal ".", bi identifier ]
			, choice "Syntactic form call" [bi identifier]
			, choice "Call of a builtin" [call "builtin"]
			, choice "Grouping an entire parsetree to a single token" [Literal "$", call "bnfTerm"]
			]
		, syntForm "bnfSeq" "A sequence of BNF-terms"
			[ choice "" [call "bnfTerm", call "bnfSeq"]
			, choice "" [call "bnfTerm"]
			]
		, syntForm "bar" "The separator of choices"
			[ choice "A simple bar" [Literal "|"]
			, choice' "Jumping to a newline" [call "nl", Literal "\t", bi whitespace, Literal "|"]
			]
		, syntForm "bnfChoices" "One or more choices in BNF"
			[ choice "" [call "bnfSeq", call "bar", call "bnfChoices"]
			, choice "" [call "bnfSeq", call "nl"]
			]
		, syntForm "assign" "The declaration operator, used to indicate the whitespace mode"
			[ choice "Assigment using implicit whitespace" [Literal "::="]
			, choice "Assignment where the programmer is responsible for whitespace" [Literal "~~="]]
		, syntForm "bnfDecl" "A line declaring bnf"
			[ choice "With comment or leading newlines" [call "nls", bi identifier, call "assign", call "bnfChoices"] 
			, choice "" [bi identifier, call "assign", call "bnfChoices"]
			]
		, syntForm "syntax" "An entire syntax declaration"
			[ choice "" [call "bnfDecl", call "syntax"]
			, choice "" [call "bnfDecl"]
			]
		, syntForm "ident" "Identifies something (such as a function, rule, relation, syntactic form, ...);\n can be fully qualified or not"
			[ choice "" [bi identifierUpper, Literal ".", call "ident"]
			, choice "" [bi identifier]
			]
		, syntForm "typeIdent"
				("Identifies a syntactic form; used as types for functions and relations.\n"++
				"Can be an identifier, fully qualified identiefier or builtin value")
			[ choice "The bottom type, when a function will never yield a result" [Literal $ snd typeBottom]
			, choice "The top type, when a function might return any type." [Literal $ snd typeTop]
			, choice "" [call "ident"]
			]
		]

typeBottom	= ([], "⊥")
typeTop		= ([], "⊤")
superType	= error "Did you mean to use typeTop?"


bi	= BuiltIn False


call nm	= RuleCall (["Syntax"], nm)

choice	:: Doc -> [BNF] -> (BNF, Doc)
choice doc bnfs
	= (Seq bnfs & injectWS & normalize, doc)

choice' doc bnfs
	= (Seq bnfs & normalize, doc)

syntForm	:: Name -> Doc -> [(BNF, Doc)] -> SyntacticForm
syntForm name information choices
	= SyntacticForm name (choices |> fst) (choices |> snd |> asInfo) (asInfo information) 


asInfo	:: Doc -> MetaInfo
asInfo doc
	= let i	= (-1) in
		MetaInfo (LocationInfo i i i i "MetaSyntax of BNF") doc 






choices'	:: String -> [Combiner a] -> Combiner a
choices'	str
	= choices (["Syntax"], str)

------------------------- CONVERSION OF THE PARSETREE OF A SYNTAX TO BNF --------------------------------



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
builtinValue	= choices' "builtin"
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




ident		:: Combiner ([Name], Name)
ident	= choices' "ident"
		[ cmb (\head (tail, nm) -> (head:tail, nm))
			capture (lit "." **> ident)
		, capture |> (,) []]


typeIdent	:: Combiner ([Name], Name)
typeIdent
	= choices' "typeIdent"
		[ {-bottom-} capture |> const typeBottom
		, {-top-} capture |> const typeTop
		, ident
		]


syntaxDecl	:: Combiner [SyntacticForm]
syntaxDecl
	= choices' "syntax"
		[ cmb (:) bnfDecl syntaxDecl
		, bnfDecl |> (:[])
		]


syntaxDecl'	:: Combiner Syntax
syntaxDecl'
	= syntaxDecl |> createSyntax





