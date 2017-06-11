{-# LANGUAGE RankNTypes, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
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
import qualified Data.Set as S
import qualified Data.List as L
import Data.Map (Map, fromList, toList)
import Data.Set (Set)

import Data.Function (fix)

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
		{ _syntax :: Map Name [(BNF, MetaInfo)]
		, _syntaxElementsOrder	:: [(Name, MetaInfo)]	-- Only for pretty printing and error detection
		}
	deriving (Show, Eq)
makeLenses ''Builtin
makeLenses ''ParserMetaInfo
makeLenses ''Syntax


syntaxChoices
	= syntax . mapped . mapped . _1

-------------------- SYNTAX TOOLS ----------------------

type Syntaxes	= Map Name Syntax
asSyntaxes	:: Syntax -> (Map Name Syntax, Name)
asSyntaxes s
	= (M.singleton "" s, "")

asSyntaxes' s
	= M.singleton "" s


existingNames	:: Syntaxes -> Set (Name, Name)
existingNames s
	= s |> get syntax |> M.keys & M.toList & unmerge & S.fromList


callExists	:: Syntaxes -> (Name, Name) -> Bool
callExists s (ns, nm)
	= isJust $ do
		synt	<- M.lookup ns s |> get syntax
		M.lookup nm synt


createSyntax	:: [(Name, ([(BNF, MetaInfo)], MetaInfo))] -> Syntax
createSyntax elements
	= let	order	= elements |> (fst &&& (snd . snd))
		dict	= elements ||>> fst & M.fromList in
		Syntax dict order & normalize


mergeSyntax		:: Syntax -> Syntax -> Either String Syntax
mergeSyntax (Syntax synt order) (Syntax synt' order')
	= inMsg "While merging two syntaxes" $
	  do	let merged	= Syntax (M.union synt synt') (order ++ order')
		_checkNoDuplicate (merged, "merge")
		return merged

mergeSyntax' a b
	= mergeSyntax a b  & either error id




--------------------- BNF STUFF--------------------------





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
{-

Checks:
- No duplicate choices exist over multiple rules
- Dead choices check (e.g. "a" | "a" "b")
-}

instance Check Syntaxes where
	check syntaxes
		= do	let syntaxes'	= M.toList syntaxes |> swap
			[ syntaxes' |> _checkNoDuplicate
			 , syntaxes' |> _checkNoTrivial
			 , syntaxes' |> _checkAllIdentsExist syntaxes 
			 ] |> allRight_ & allRight_
			_checkLeftRecursion syntaxes
			-- Cycles in the supertype relationship: check unneeded, prevented by left recursion check




-- | A syntactic form should not be declared twice
-- >>> import LanguageDef.Syntax
-- >>> _checkNoDuplicate (asSyntaxUnchecked' "tests" "\nabc ::= Number\nabc ::= Number\n", "")
-- Left ...
-- >>> _checkNoDuplicate (asSyntaxUnchecked' "tests" "\nabc ::= Number\n", "")
-- Right ...
_checkNoDuplicate	:: (Syntax, Name) -> Either String ()
_checkNoDuplicate (Syntax synts order, nm)
	= inMsg ("While checking the syntax or "++nm) $ 
	  checkNoDuplicates (order |> fst) $ \dupNames ->
	  let 	dupMeta	= dupNames |> (id &&& flip lookup order) |> sndEffect & catMaybes
		dupEntries
			= dupMeta |> (\(nm, meta) -> nm ++ "\t "++toCoParsable meta) & unlines
		in
		"Some syntactic forms are declared multiple times:" ++ indent dupEntries
		

		


-- | All rulecalls shoud exist
-- >>> import LanguageDef.Syntax
-- >>> let syntax1 = asSyntaxUnchecked' "tests" "\nabc ::= x\n"
-- >>> _checkAllIdentsExist (asSyntaxes' syntax1) (syntax1, "")
-- Left ...
-- >>> let syntax2 = asSyntaxUnchecked' "tests" "\nabc ::= abc\n"
-- >>> _checkAllIdentsExist (asSyntaxes' syntax2) (syntax2, "")
-- Right ...
_checkAllIdentsExist	:: Syntaxes -> (Syntax, Name) -> Either String ()
_checkAllIdentsExist otherSyntaxes (syntx, localScope)
	= let	existingFQ	= existingNames otherSyntaxes
		called		= syntx & syntaxRuleCalls localScope
					:: [((MetaInfo, Name), (Name, Name))]
		missing	= called & filter ((`S.notMember` existingFQ) . snd)
		printMissing ((mi, nm), (ns, called))
			= ns ++ "." ++ called ++"\t (used in the declaration of "++nm++", "++toCoParsable (get miLoc mi) ++")"
		in
		assert (null missing) $ 
			"Some used syntactic forms are not known: "++
			(missing |> printMissing |> indent & unlines)
		

-- | Rules are not trivial (= one choice with only the rulecall)
-- >>> import LanguageDef.Syntax
-- >>> let syntax2 = asSyntaxUnchecked' "tests" "\nabc ::= abc\n"
-- >>> _checkNoTrivial (syntax2, "")
-- Left ...
_checkNoTrivial	:: (Syntax, Name) -> Either String ()
_checkNoTrivial (synt, nm)
	= let	trivial	= synt & get syntax & M.toList 
				& filter ((==) 1 . length . snd)
				& unmerge	-- does not have a real effect, all the remaining lists have length 1
				& filter (isRuleCall . fst . snd)	-- Now only trivial stuff remains
		printTriv (nm, (bnf, mi))
			= nm++" = "++toParsable bnf++"\t (declared "++toCoParsable (get miLoc mi)++")"
		in
		inMsg ("In the syntax of "++nm) $
		assert (null trivial) $
			"Some syntactic forms are trivial, remove them: "++
				trivial |> printTriv |> indent & unlines



-- | No left recursion exists
-- >>> import LanguageDef.Syntax
-- >>> let synt0 = asSyntaxUnchecked' "test" "\nabc ::= def\ndef ::= abc\n"
-- >>> _checkLeftRecursion (asSyntaxes' synt0)
-- Left ...
-- >>> let synt1 = asSyntaxUnchecked' "test" "\nx ::= x\n"
-- >>> _checkLeftRecursion (asSyntaxes' synt1)
-- Left ...
-- >>> let synt2 = asSyntaxUnchecked' "test" "\nx ::= x \"a\"\n"
-- >>> _checkLeftRecursion (asSyntaxes' synt2)
-- Left ...
_checkLeftRecursion	:: Syntaxes -> Either String ()
_checkLeftRecursion syntaxes
  = do	let lr	= leftRecursiveCalls syntaxes
			& existingNames & S.toList
	-- only the left recursive stuff is left by the algo
	assert (null lr) $ "Some left recursive rules are left: "
		++ (lr |> showIdent' |> indent & unlines) 


-- | Calculates the left recursive calls in all syntaxes
-- Only values left will be left recursive
-- >>> import LanguageDef.Syntax
-- >>> let synt = asSyntaxUnchecked' "test" "\nabc ::= def\ndef ::= abc\n" & asSyntaxes'
-- >>> leftRecursiveCalls synt & M.toList & head & snd & toParsable
-- "\n\nabc\t::= .def\n\n\ndef\t::= .abc\n"
leftRecursiveCalls	:: Syntaxes -> Syntaxes
leftRecursiveCalls syntaxes
 = let  -- Remove all but the first element of all choices
	syntaxes' = syntaxes |> over syntaxChoices removeTail
	syntaxes'' = syntaxes'
			-- only keep choices that start with a rulecall
			  |> over (syntax . mapped) (L.filter (isRuleCall . fst))
			  & M.mapWithKey fullyQualify	-- no pesky local calls!

	fqCall (RuleCall (Just ns) nm)
		= (ns, nm)

	-- remove syntactic forms with no resting choices from the syntaxes
	cleanSynt syntaxes
		= syntaxes |> over syntax (M.filter (not . L.null))
	-- remove rules for which the syntactic forms don't exist anymore
	removeEmpty syntaxes
		= syntaxes |> over (syntax . mapped) (L.filter (callExists syntaxes . fqCall . fst))

	in
	fixLoop syntaxes'' (cleanSynt . removeEmpty)



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

usedSyntacticForms	:: BNF -> [(Maybe Name, Name)]
usedSyntacticForms (RuleCall ns name)
		= [(ns, name)]
usedSyntacticForms (Seq bnfs)
		= bnfs >>= usedSyntacticForms
usedSyntacticForms _
		= []



isRuleCall	:: BNF -> Bool
isRuleCall (RuleCall {})
		= True
isRuleCall _	= False


getRuleCall	:: BNF -> Maybe (Maybe Name, Name)
getRuleCall (RuleCall ns nm)
		= Just (ns, nm)
getRuleCall _ 	= Nothing

getRuleCalls	:: BNF -> [(Maybe Name, Name)]
getRuleCalls (Seq bnfs)
		= bnfs >>= getRuleCalls
getRuleCalls (RuleCall ns nm)
		= [(ns, nm)]
getRuleCalls _	= []



-- | All Fully Qualified calls, with the first param the namespace for local calls. Metainfo is the choice in which the call is made, name is the declaration for which the call is made
syntaxRuleCalls	:: Name -> Syntax -> [((MetaInfo, Name), (Name, Name))]
syntaxRuleCalls localScope s
	= let	synt	= get syntax s
		extractBNF bnf
			= bnf & getRuleCalls |> over _1 (fromMaybe localScope)
		in
		synt & M.toList & unmerge
			|> (\(name, (bnf, mi)) -> ((mi, name), extractBNF bnf))
			& unmerge



fullyQualify	:: Name -> Syntax -> Syntax
fullyQualify localScope s
	= s & over syntaxChoices (fullyQualifyBNF localScope)

fullyQualifyBNF	:: Name -> BNF -> BNF
fullyQualifyBNF ns (RuleCall Nothing nm)
	= RuleCall (Just ns) nm
fullyQualifyBNF ns (Seq bnfs)
	= Seq (bnfs |> fullyQualifyBNF ns)
fullyQualifyBNF ns (Group bnf)
	= Group $ fullyQualifyBNF ns bnf
fullyQualifyBNF _ bnf
	= bnf


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
		= synt & over (syntax . mapped . mapped . _1) normalize





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
		= order |> ((fst &&& (flip M.lookup syntax . fst)) &&& snd)
			|> unmerge3l
			|> _showForm & unlines

_showForm	:: (Name, Maybe [(BNF, MetaInfo)], MetaInfo) -> String
_showForm (nm, Nothing, mi)	= error $ "Huh? No entry in syntax for "++nm
_showForm (nm, Just choices, meta)
	= let	docStr	= toParsable meta
		assgn	= if all (\(bnf, _) -> containsHidden bnf || isSingle bnf) choices then "::=" else "~~="
		header	= nm ++ "\t"++ assgn ++" "
		choices'	= choices |> (\(bnf, meta) -> toParsable (removeWS bnf) ++toCoParsable meta) & intercalate "\n\t | "
		in
		["", docStr, header ++ choices'] & intercalate "\n"


