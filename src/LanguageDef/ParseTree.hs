{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
module LanguageDef.ParseTree where

{- 
The data structure representing a parsetree and related
 -}

import Utils.All

import LanguageDef.Syntax hiding (Literal, Seq, string)
import LanguageDef.LocationInfo
import qualified LanguageDef.Syntax as BNF

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe 

import Text.Parsec
import Data.Bifunctor

import Control.Monad

import Debug.Trace


data ParseTree a
	= Literal	{ _ptToken :: String	, _ptLocation :: LocationInfo, _ptA :: a, _ptHidden :: Bool}
	| Int 		{ _ptInt :: Int		, _ptLocation :: LocationInfo, _ptA :: a}
	| Seq 		{ _pts :: [ParseTree a]	, _ptLocation :: LocationInfo, _ptA :: a}
	| RuleEnter	{ _pt	:: ParseTree a , _ptUsedRule :: Name, _ptUsedIndex :: Int , _ptLocation :: LocationInfo, _ptA :: a}
	deriving (Show, Ord, Eq)

makeLenses ''ParseTree

type ParseTree'	= ParseTree ()


instance Normalizable (ParseTree a) where
	normalize (Seq [pt] _ _)
		= pt
	normalize (RuleEnter pt nm i loc a)
		= RuleEnter (normalize pt) nm i loc a
	normalize pt =  pt 

contents	:: ParseTree a -> String
contents (Literal token _ _ _)
	= token
contents (Int i _ _)
	= show i
contents (Seq pts _ _)
	= pts >>= contents
contents (RuleEnter pt _ _ _ _)
	= contents pt


isHiddenPt	:: ParseTree a -> Bool
isHiddenPt (Literal _ _ _ hidden)
	= hidden
isHiddenPt _
	= False

removeHidden	:: ParseTree a -> ParseTree a
removeHidden (Seq pts l a)
	= let pts'	= pts & filter (not . isHiddenPt) |> removeHidden in
		Seq pts' l a
removeHidden re@(RuleEnter pt nm i location a)
	= let	pt'	= pt & removeHidden in
		RuleEnter pt' nm i location a
removeHidden pt
	= pt

---------------------- COMBINATOR STUFF --------------------------------


data Combiner a	= LiteralC (String -> Either String a)
		| forall b c . SeqC (Combiner b) (Combiner c) (b -> c -> a)
		| forall b . MapC (Combiner b) (b -> a)
		| IntC (Int -> Either String a)
		| Value a
		| Annot Name [Combiner a]	-- Enter a certain rule, with one combiner for each choice
		| forall b . WithLocation (Combiner b) (LocationInfo -> b -> a)

instance Show  (Combiner a) where
	show c	= _show 5 c


_show	:: Int -> Combiner a -> String
_show 0	_ 
	= " ... "
_show _ (LiteralC _)
	= "LiteralC"
_show i (SeqC a b _)
	= "SeqC "++inParens (inParens (_show (i-1) a) ++" "++ _show (i-1) b)
_show _ (IntC _)
	= "IntC"
_show _ (Value _)
	= "Value"
_show i (MapC b _)
	= _show i b
_show i (Annot nm combiners)
	= "Annot "++show nm++" "++(combiners |> _show (i - 1) & commas & inParens')
_show i (WithLocation cb _)
	= _show i cb

instance Functor Combiner where
	fmap f ca	
		= MapC ca f


interpret	:: (Show x) => Combiner a -> ParseTree x -> Either String a
interpret (LiteralC fa) (Literal str' _ _ _)
 	= fa str'
interpret combiner (Seq [pt] _ _)
	= interpret combiner pt
interpret (SeqC ac bc f) sq@(Seq (p:ptss) _ _)
	= do	a	<- interpret ac p
		b	<- interpret bc $ set pts ptss sq
		return $ f a b
interpret (MapC ca f) pt
	= do	a	<- interpret ca pt
		return $ f a
interpret (IntC f) (Int i _ _)
	= f i
interpret (Value a) _
	= return a
interpret (Annot ruleName cmber) (RuleEnter pt' ruleName' choice _ _)
	= inMsg ("While interpreting a combiner for "++ruleName) $
          do	unless (ruleName == ruleName') $ Left $ "Assertion failed: could not interpret, expected construction with "++show ruleName++" but got a "++show ruleName'
		interpret (cmber !! choice) pt'
interpret (WithLocation cb flib2a) pt
	= do	let li	= get ptLocation pt
		b	<- interpret cb pt
		return $ flib2a li b
interpret combiner parsetree
	= Left $ "Could not interpret "++show combiner++" over "++debug parsetree


capture	:: Combiner String
capture = LiteralC Right

int	:: Combiner Int
int	= IntC Right

lit	:: String -> Combiner String
lit str	= LiteralC (\str' -> if str == str' then Right str else Left $ "Expected literal string "++show str++", but got "++show str')

just	:: a -> Combiner a
just a
	= LiteralC $ const $ Right a

cmb	:: (b -> c -> a) -> Combiner b -> Combiner c -> Combiner a
cmb f cb cc
	= SeqC cb cc f

infixr <**
(<**)	:: Combiner b -> Combiner c -> Combiner b
(<**)	= cmb const

infixr **>
(**>)	:: Combiner b -> Combiner c -> Combiner c
(**>)	= cmb seq



choices	:: String -> [Combiner a] -> Combiner a
choices = Annot

withLocation	:: (LocationInfo -> a -> b) -> Combiner a -> Combiner b
withLocation f ca
	= WithLocation ca f

--------------------- PARSER STUFF --------------------------------------


parse	:: Name -> Syntax -> Name -> String -> Either String ParseTree'
parse fileName syntax syntacticForm string
	= _parse fileName (_parseRule syntax syntacticForm) string

parseMany	:: Name -> Syntax -> Name -> String -> Either String [ParseTree']
parseMany fileName syntax syntacticForm string
	= _parse fileName (many $ _parseRule syntax syntacticForm) string

_parse	:: Name -> Parser a -> String -> Either String a
_parse fileName parser input
	= _runParser fileName parser input


_runParser	:: Name -> Parser a -> String -> Either String a
_runParser fileName parser string
	= runParser parser (ParserMetaInfo fileName 0 0) fileName string
		& first show


_parseRule	:: Syntax -> Name -> Parser ParseTree'
_parseRule s@(Syntax syntax _) nm
	= do	choices	<- checkExists nm syntax ("Syntactic form "++nm++" not found in the syntax")
				& either fail return
				|> fst ||>> fst
		choice (mapi choices |> _parseChoice s nm) <?> nm

_parseChoice	:: Syntax -> Name -> (Int, BNF) -> Parser ParseTree'
_parseChoice syntax nm (i, choice)
	= try $
	  do	start	<- location
		pt	<- _parseBNF syntax choice
		info	<- endToken start
		return $ RuleEnter pt nm i info ()
		


_parseBNF	:: Syntax -> BNF -> Parser ParseTree'
_parseBNF syntax (BNF.Literal str)
		= do	start	<- location
			string str
			skipChars str
			info	<- endToken start
			return $ Literal str info () False
			
_parseBNF syntax (BNF.BuiltIn hidden builtin)
		= do	start	<- location
			parsed	<- get biParse builtin
			skipChars (either id show parsed)
			info	<- endToken start
			return $ case parsed of
				Left str -> Literal str info () hidden
				Right i  -> Int i info ()

_parseBNF syntax (BNF.RuleCall name)
		= _parseRule syntax name

_parseBNF syntax (BNF.Group bnf)
		= do	start	<- location
			pt	<- _parseBNF syntax bnf
			info	<- endToken start
			return $ Literal (contents pt) info () False


_parseBNF syntax (BNF.Seq bnfs)
		= do	start	<- location
			pts	<- bnfs |+> _parseBNF syntax
			info	<- endToken start
			return $ Seq pts info ()



endToken	:: (Int, Int) -> Parser LocationInfo
endToken (lStart, cStart)
	= do	(lEnd, cEnd)	<- location
		file	<- getState |> get pmiFile
		return $ LocationInfo lStart cStart lEnd cEnd file

location	:: Parser (Int, Int)
location = do	st	<- getState
		return (get pmiLine st, get pmiColumn st)

skipChars	:: String -> Parser ()
skipChars []
	= return ()
skipChars ('\n':cs)
	= nextLine >> skipChars cs
skipChars (c:cs)
	= modifyState (over pmiColumn (+1)) >> skipChars cs

-- Increments linecount
nextLine	:: Parser ()
nextLine = modifyState (
		over pmiLine (+1) .
		set pmiColumn 0)


instance ToString (ParseTree a) where
	toParsable (Literal str _ _ _)
		= str
	toParsable (Int i _ _)
		= show i
	toParsable (Seq s _ _)
		= s >>= toParsable
	toParsable (RuleEnter pt _ _ _ _)
		= toParsable pt

	toCoParsable (Seq s _ _)
		= s |> toCoParsable |>>= lines |> (" | " ++) & intercalate "\n"
	toCoParsable (RuleEnter pt _ _ _ _)
		= toCoParsable pt
	toCoParsable pt
		= toParsable pt

	debug (Literal str _ _ _)
		= show str
	debug (Int i _ _)
		= show i
	debug (Seq pts _ _)
		= (pts |> debug & commas & inParens')
	debug (RuleEnter pt name choice _ _)
		= (debug pt & inParens) ++ (name++"."++show choice)

