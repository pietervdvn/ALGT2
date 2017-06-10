{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module LanguageDef.Syntax.ParseTree where

{- 
The data structure representing a parsetree and related
 -}

import Utils.All

import LanguageDef.Syntax.Syntax hiding (Literal, Seq, string)
import LanguageDef.LocationInfo
import qualified LanguageDef.Syntax.Syntax as BNF

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe 

import Text.Parsec
import Data.Bifunctor

import Control.Monad


data ParseTree a
	= Literal	{ _ptToken :: String	, _ptLocation :: LocationInfo, _ptA :: a, _ptHidden :: Bool}
	| Int 		{ _ptInt :: Int		, _ptLocation :: LocationInfo, _ptA :: a}
	| Seq 		{ _pts :: [ParseTree a]	, _ptLocation :: LocationInfo, _ptA :: a}
	| RuleEnter	{ _pt	:: ParseTree a , _ptUsedRule :: Name, _ptUsedIndex :: Int , _ptLocation :: LocationInfo, _ptA :: a}
	deriving (Show, Ord, Eq, Functor)

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


--------------------- PARSER STUFF --------------------------------------


parse	:: FilePath -> (Syntaxes, Name) -> Name -> String -> Either String ParseTree'
parse fileName syntax syntacticForm string
	= _parse fileName (_parseRule syntax syntacticForm <* eof) string

parseMany	:: FilePath -> (Syntaxes, Name) -> Name -> String -> Either String [ParseTree']
parseMany fileName syntax syntacticForm string
	= _parse fileName (many $ _parseRule syntax syntacticForm) string

_parse	:: FilePath -> Parser a -> String -> Either String a
_parse fileName parser input
	= _runParser fileName parser input


_runParser	:: FilePath -> Parser a -> String -> Either String a
_runParser fileName parser string
	= runParser parser (ParserMetaInfo fileName 0 0) fileName string
		& first show


_parseRule	:: (Syntaxes, Name) -> Name -> Parser ParseTree'
_parseRule s@(syntaxes, ns) nm
	= do	(Syntax syntax _)	<- checkExists ns syntaxes ("No namespace "++ns++" found") & either fail return
		choices	<- checkExists nm syntax ("Syntactic form "++nm++" not found in the syntax")
				& either fail return
				|> fst ||>> fst
		choice (mapi choices |> _parseChoice s nm)

_parseChoice	:: (Syntaxes, Name) -> Name -> (Int, BNF) -> Parser ParseTree'
_parseChoice syntaxes nm (i, choice)
	= try $
	  do	start	<- location
		pt	<- _parseBNF syntaxes choice
		info	<- endToken start
		return $ RuleEnter pt nm i info ()
		


_parseBNF	:: (Syntaxes, Name) -> BNF -> Parser ParseTree'
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

_parseBNF (syntaxes, defNs) (BNF.RuleCall ns name)
		= _parseRule (syntaxes, fromMaybe defNs ns) name

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

