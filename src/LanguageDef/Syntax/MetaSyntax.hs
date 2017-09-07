module LanguageDef.Syntax.MetaSyntax where

{- 

Defines the BNF of BNF, used to parse BNF

 -}

import Utils.All

import LanguageDef.Syntax.Syntax
import LanguageDef.Syntax.BNF
import LanguageDef.Syntax.ParseTree (ParseTree, parse)
import LanguageDef.LocationInfo

import qualified Data.Map as M



------------------------- DEFINITION OF BNF IN BNF -------------------------------------------

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
		[ syntForm "builtins" "The names of syntactic forms that are available as builtins"
			(knownBuiltins |> (\bisf -> choice (get biDocs bisf++"; "++get biRegex bisf) [Literal $ get biName bisf] ))
		, syntForm "bnfTerm" "A single term of BNF, thus either a literal, syntactic form call or builtin"
			[ choice "Literal value" [bi string]
			, choice "Syntactic form call in some namespace" [bi identifierUpper, Literal ".", bi identifier ]
			, choice "Syntactic form call" [bi identifier]
			, choice "Call of a builtin" [call "builtins"]
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
		]

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
