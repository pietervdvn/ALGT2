module LanguageDef.MetaSyntax where

{- 

Defines the BNF of BNF, used to parse BNF

 -}

import Utils.All

import LanguageDef.Syntax
import LanguageDef.ParseTree (ParseTree, parse, parseMany)
import LanguageDef.LocationInfo

import qualified Data.Map as M



------------------------- DEFINITION OF BNF IN BNF -------------------------------------------

bnfSyntax	:: Syntax
bnfSyntax	
	= createSyntax $ 
		[ syntForm "commentContents" "The contents of comments, such as what you are reading right now"
			[ choice "Body of the comment" [bi lineChar, RuleCall "commentContents"]
			, choice "Closing of the comment" [Literal "\n"]
			]
		, syntForm "comment" "A comment"
			[ choice "" [Literal "#", Group ( RuleCall "commentContents")]]
		, syntForm "nl" "A newline, possibly with a single comment"
			[ choice "" [RuleCall "comment"]
			, choice "" [Literal "\n"]
			]
		, syntForm "nls" "Multiple nls, possibly with comments"
			[ choice "" [RuleCall "nl", RuleCall "nls"]
			, choice "" [RuleCall "nl"]
			]
		, syntForm "builtins" "The names of syntactic forms that are available as builtins"
			(knownBuiltins |> (\bisf -> choice (get biDocs bisf++"; "++get biRegex bisf) [Literal $ get biName bisf] ))
		, syntForm "bnfTerm" "A single term of BNF, thus either a literal, syntactic form call or builtin"
			[ choice "Literal value" [bi string]
			, choice "Syntactic form call" [bi identifier]
			, choice "Call of a builtin" [RuleCall "builtins"]
			, choice "Grouping an entire parsetree to a single token" [Literal "$", RuleCall "bnfTerm"]
			]
		, syntForm "bnfSeq" "A sequence of BNF-terms"
			[ choice "" [RuleCall "bnfTerm", RuleCall "bnfSeq"]
			, choice "" [RuleCall "bnfTerm"]
			]
		, syntForm "bar" "The separator of choices"
			[ choice "A simple bar" [Literal "|"]
			, choice' "Jumping to a newline" [RuleCall "nl", Literal "\t", bi whitespace, Literal "|"]
			]
		, syntForm "bnfChoices" "One or more choices in BNF"
			[ choice "" [RuleCall "bnfSeq", RuleCall "bar", RuleCall "bnfChoices"]
			, choice "" [RuleCall "bnfSeq", RuleCall "nl"]
			]
		, syntForm "assign" "The declaration operator, used to indicate the whitespace mode"
			[ choice "Assigment using implicit whitespace" [Literal "::="]
			, choice "Assignment where the programmer is responsible for whitespace" [Literal "~~="]]
		, syntForm "bnfDecl" "A line declaring bnf"
			[ choice "" [RuleCall "nls", bi identifier, RuleCall "assign", RuleCall "bnfChoices"] ]
		, syntForm "syntax" "An entire syntax declaration"
			[ choice "" [RuleCall "bnfDecl", RuleCall "syntax"]
			, choice "" [RuleCall "bnfDecl"]
			]
		]

bi	= BuiltIn False

choice	:: Doc -> [BNF] -> (BNF, Doc)
choice doc bnfs
	= (Seq bnfs & injectWS & normalize, doc)

choice' doc bnfs
	= (Seq bnfs & normalize, doc)

syntForm	:: Name -> Doc -> [(BNF, Doc)] -> (Name, ([(BNF, MetaInfo)], MetaInfo))
syntForm name information choices
	= (name, (choices |> over _2 asInfo, asInfo information))


asInfo	:: Doc -> MetaInfo
asInfo doc
	= let i	= (-1) in
		MetaInfo (LocationInfo i i i i "MetaSyntax of BNF") doc
