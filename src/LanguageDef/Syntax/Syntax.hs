{-# LANGUAGE RankNTypes, TemplateHaskell #-}
module LanguageDef.Syntax.Syntax where

{- 

The defintion of
 - Syntaxes
 - Location and meta info
 - BNF

 - BNF-builtins
 - The parser type used (ParsecT) + its state

 -}

import Utils.All

import LanguageDef.LocationInfo

import Text.Parsec (Parsec, char, oneOf, noneOf, many, many1, (<|>), try)
import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map, fromList, toList)

import Control.Arrow ((&&&))

import Lens.Micro.TH
import Lens.Micro (mapped)



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



data BNF	= Literal String	-- Parse literally this string
		| BuiltIn Bool Builtin	-- Run a builtin parser, eventually 'hidden' from some views
		| RuleCall (Maybe Name) Name	-- Call another rule, possibly in another namespace
		| Group BNF
		| Seq [BNF]		-- Sequence of elements
	deriving (Show, Eq)






data Syntax = Syntax
		{ _syntax :: Map Name ([(BNF, MetaInfo)], MetaInfo)
		, _syntaxElementsOrder	:: [Name]	-- Only for pretty printing
		}
	deriving (Show, Eq)
makeLenses ''Builtin
makeLenses ''ParserMetaInfo
makeLenses ''Syntax


type Syntaxes	= Map Name Syntax
asSyntaxes	:: Syntax -> (Map Name Syntax, Name)
asSyntaxes s
	= (M.singleton "" s, "")



createSyntax	:: [(Name, ([(BNF, MetaInfo)], MetaInfo))] -> Syntax
createSyntax elements
	= let	order	= elements |> fst
		dict	= elements & M.fromList in
		Syntax dict order & normalize


mergeSyntax		:: Syntax -> Syntax -> Either String Syntax
mergeSyntax (Syntax synt order) (Syntax synt' order')
	= inMsg "While merging two syntaxes" $
	  do	let showEntry (nm, (_,mi))	= nm++" "++inParens (get miLoc mi & toParsable)
		let mergeCommons common	= common >>= (\(k, (v, v')) -> [(k, v), (k, v')])
		checkNoCommon synt synt' (\common -> "Common elements exists: "++
			(common & mergeCommons |> showEntry & unlines) )
		return (Syntax (M.union synt synt') (order ++ order'))

mergeSyntax' a b
	= mergeSyntax a b  & either error id









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

knownBuiltins
	= 	[whitespace
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
		(do	h <- oneOf lowers
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


-------------------------------- CHECKS -------------------------------------

-- TODO port the checks!




-------------------------------- TOOLS -------------------------------------


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

usedSyntacticForms	:: BNF -> [(Maybe Name, Name)]
usedSyntacticForms (RuleCall ns name)
		= [(ns, name)]
usedSyntacticForms (Seq bnfs)
		= bnfs >>= usedSyntacticForms
usedSyntacticForms _
		= []



-------------------------------- UTILS -------------------------------------






instance Normalizable BNF where
	normalize (Seq bnfs)
		= bnfs |> normalize |>>= _unpack & Seq
	normalize bnf
		= bnf


_unpack	:: BNF -> [BNF]
_unpack (Seq bnfs)
		= bnfs
_unpack bnf	= [bnf]


instance Normalizable Syntax where
	normalize synt
		= synt & over (syntax . mapped . _1 . mapped . _1) normalize




-------------------------------- TOSTRING -------------------------------------


instance ToString Builtin where
	toParsable 	= get biName




instance ToString BNF where
	toParsable (Literal string)
			= show string
	toParsable (BuiltIn _ b)
			= toParsable b
	toParsable (RuleCall Nothing name)
			= name
	toParsable (RuleCall (Just ns) name)
			= ns ++"." ++ name
	toParsable (Group bnf)
			= "$"++toParsable bnf
	toParsable (Seq bnfs)
			= bnfs |> toParsable & unwords


instance ToString Syntax where
	toParsable (Syntax syntax order)
		= let	dictShown	= syntax & toList |> (fst &&& showForm) & fromList in
			order |> flip M.lookup dictShown |> fromJust & intercalate "\n"

showForm	:: (Name, ([(BNF, MetaInfo)], MetaInfo)) -> String
showForm (nm, (choices, meta))
	= let	docStr	= toParsable meta
		assgn	= if all (\(bnf, _) -> containsHidden bnf || isSingle bnf) choices then "::=" else "~~="
		header	= nm ++ "\t"++ assgn ++" "
		choices'	= choices |> (\(bnf, meta) -> toParsable (removeWS bnf) ++toCoParsable meta) & intercalate "\n\t | "
		in
		[docStr, header ++ choices'] & unlines


