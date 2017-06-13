{-# LANGUAGE TemplateHaskell #-}
module LanguageDef.Syntax.BNF where


import Utils.All

import LanguageDef.LocationInfo
import Data.Maybe

import Text.Parsec (Parsec, char, oneOf, noneOf, many, many1, (<|>), try)


type FQName	= ([Name], Name)

showFQ (ns, nm)
		= (ns ++ [nm]) & intercalate "."

{- Representation of BNF-expressions. They are a part of the context free grammer notation -}
data BNF	= Literal String	-- Parse literally this string
		| BuiltIn Bool Builtin	-- Run a builtin parser, eventually 'hidden' from some views
		| RuleCall FQName	-- Call another rule, possibly in another namespace
		| Group BNF
		| Seq [BNF]		-- Sequence of elements
	deriving (Show, Eq)


data ParserMetaInfo
	= ParserMetaInfo 
		{ _pmiFile	:: String
		, _pmiLine	:: Int
		, _pmiColumn	:: Int}
type Parser r	= Parsec String ParserMetaInfo r


data Builtin = Builtin 
		{ _biName	:: Name
		, _biDocs	:: Doc
		, _biRegex	:: String
		, _biParse	:: Parser (Either String Int)}
instance Show Builtin where
	show (Builtin n d reg _)
		= ["Builtin ", show n, show d, show reg] & unwords
instance Eq Builtin where
	(==) (Builtin n d r _) (Builtin n' d' r' _)
		= n == n' && d == d' && r == r'

makeLenses ''Builtin
makeLenses ''ParserMetaInfo


-- Injects ws everywhere between sequences. Leading and trailing whitespace is **not** handled
injectWS	:: BNF -> BNF
injectWS (Seq bnfs)
	= bnfs & intersperse (BuiltIn True whitespace) & Seq
injectWS bnf
	= bnf

removeWS (Seq bnfs)
	= bnfs & filter (/= BuiltIn True whitespace) & Seq
removeWS bnf
	= bnf


removeTail	:: BNF -> BNF
removeTail (Group bnf)
	= removeTail bnf
removeTail (Seq (bnf:bnfs))
	= removeTail bnf
removeTail bnf
	= bnf





containsHidden	:: BNF -> Bool
containsHidden (Seq bnfs)
	= bnfs & any isHidden
containsHidden bnf	= isHidden bnf


isHidden	:: BNF -> Bool
isHidden (BuiltIn hidden _)
		= hidden
isHidden _ 	= False

isSingle	:: BNF -> Bool
isSingle (Seq [bnf])
		= True
isSingle (Seq _)	= False
isSingle _	= True

usedSyntacticForms	:: BNF -> [FQName]
usedSyntacticForms 	= getRuleCalls


isRuleCall	:: BNF -> Bool
isRuleCall (RuleCall {})
		= True
isRuleCall _	= False


getRuleCall	:: BNF -> Maybe FQName
getRuleCall (RuleCall nm)
		= Just nm
getRuleCall _ 	= Nothing

getRuleCalls	:: BNF -> [FQName]
getRuleCalls (Seq bnfs)
		= bnfs >>= getRuleCalls
getRuleCalls (RuleCall nm)
		= [nm]
getRuleCalls _	= []




------------------------------ BUILTIN STUFF + HELPERS -------------------------


digits	= ['0'..'9']
lowers	= ['a'..'z']
uppers	= ['A'..'Z']
hexDigits
	= digits ++ ['a'..'f'] ++ ['A'..'F']

builtinEscapes	:: [((Char, Char), String)]
builtinEscapes
      =	[ (('n', '\n'), "newline")
	, (('t', '\t'), "tab")
	, (('"', '"'), "double quote")
	, (('\\', '\\'), "backslash")
	]
builtinEscapes'
	= builtinEscapes |> fst



knownBuiltins
	= 	[whitespace
		, identifierUpper
		, identifier
		, string
		, anyChar
		, upperChar
		, lowerChar
		, digitChar
		, lineChar
		, wordChar
		, intBI
		, numberBI]

whitespace
	= Builtin "Whitespace" "Zero or more whitespace characters. Always produces a token in the parsetree" "[ \\t]*" (_ws |> Left)

identifier
	= Builtin "Identifier" "An identifier starting with a lowercase letter" "[a-z][a-zA-Z0-9]*" 
		(do	h	<- oneOf lowers
			tail	<- many (oneOf (lowers ++ uppers ++ digits))
			return $ Left (h:tail))

identifierUpper
	= Builtin "IdentifierUpper" "An identifier starting with an uppercase letter" "[A-Z][a-zA-Z0-9]*"
		(do	h	<- oneOf uppers
			tail	<- many (oneOf (lowers ++ uppers ++ digits))
			return $ Left (h:tail))

string	= Builtin "String" "A double quoted string, where \\\" is a literal double quote and \\\\ is a literal backslash" ("\"([^\"]|\\["++(builtinEscapes' |> fst) ++"])\"")
		(_dqString' |> Left)

anyChar		= Builtin "Any" "Any single character" "." (_single $ noneOf "")
upperChar	= Builtin "Upper" "Any upper character" "[A-Z]" (_single $ oneOf uppers)
lowerChar	= Builtin "Lower" "Any lower character" "[a-z]" (_single $ oneOf lowers)
digitChar	= Builtin "Digit" "Any digit" "[0-9]" (_single $ oneOf digits)

lineChar	= Builtin "LineChar" "Any single character that is not a newline. This includes \\r." "[^\\n]" (_single $ noneOf "\n")
wordChar	= Builtin "WordChar" "Any single character that is not a whitespace or newline" "[^ \\t\\n]" (_single $ noneOf "\n")

intBI	= Builtin "Integer" "Matches an (possibly negative) integer. Integers parsed by this might be passed into the builtin arithmetic functions." "-?[0-9]+"
			(_negNumber |> Right)

numberBI	= Builtin "Number" "Matches an positive number. Integers parsed by this might be passed into the builtin arithmetic functions." "[0-9]+"
			(_number |> Right)

		
_single		:: Parser Char -> Parser (Either String Int)
_single p	= p |> (:[]) |> Left

_parseEscape	:: Parser String
_parseEscape
	= do	builtinEscapes' |> (\(inp, _) -> char inp >> return ('\\':inp:[]))
			& foldr1 (<|>)

_dqString	:: Parser String
_dqString	
	= do	char '"'
		str <- many1 (noneOf "\\\"" |> (:[]) <|> 
					(char '\\' >> _parseEscape))
		char '"'
		return $ concat str 

-- Same as dqString, but does return the double quotes
_dqString'	:: Parser String
_dqString'
	= do	s	<- _dqString
		return $ "\""++s++"\""

_number	:: Parser Int
_number 	= many1 (oneOf digits) |> read

_negNumber	:: Parser Int
_negNumber	= do	sign	<- try (char '-' >> return negate) <|> return id
			i	<- _number
			return $ sign i

_ws	= many $ oneOf " \t"


_unescape	:: String -> String
_unescape []	= []
_unescape ('\\':c:cs)
	= let	msg	= "Invalid escape \\"++[c]
		c'	= builtinEscapes' & lookup c 
				& fromMaybe (error msg)
		in
		c' : _unescape cs
_unescape (c:cs)
		= c : _unescape cs


unescape	:: String -> String
unescape str	= _unescape $ init $ tail str

------------------------ UTILS -------------------

instance Normalizable BNF where
	normalize (Seq bnfs)
		= bnfs |> normalize |>>= _unpack & Seq
	normalize bnf
		= bnf


_unpack	:: BNF -> [BNF]
_unpack (Seq bnfs)
		= bnfs
_unpack bnf	= [bnf]



instance ToString Builtin where
	toParsable 	= get biName




instance ToString BNF where
	toParsable (Literal string)
			= show string
	toParsable (BuiltIn _ b)
			= toParsable b
	toParsable (RuleCall fq)
			= showFQ fq
	toParsable (Group bnf)
			= "$"++toParsable bnf
	toParsable (Seq bnfs)
			= bnfs |> toParsable & unwords
