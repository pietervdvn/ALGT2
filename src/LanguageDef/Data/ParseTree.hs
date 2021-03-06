{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module LanguageDef.Data.ParseTree where

{- 
The data structure representing a ParseTree' and related
 -}

import Utils.All

import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.Grouper

import LanguageDef.Data.SyntFormIndex
import LanguageDef.Data.BNF (Parser, BNF, ParserMetaInfo (ParserMetaInfo), pmiColumn, pmiLine, pmiFile, biParse)
import qualified LanguageDef.Data.BNF as BNF
import LanguageDef.Data.SyntacticForm

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe 

import Text.Parsec
import Data.Bifunctor

import Control.Monad


data ParseTree' a
	= Literal	{ _ptToken :: String	, _ptLocation :: LocationInfo, _ptA :: a, _ptHidden :: Bool}
	| Int 		{ _ptInt :: Int		, _ptLocation :: LocationInfo, _ptA :: a}
	| Seq 		{ _pts :: [ParseTree' a]	, _ptLocation :: LocationInfo, _ptA :: a}
	| RuleEnter	{ _pt	:: ParseTree' a  , _ptUsedRule :: FQName, _ptUsedIndex :: Int , _ptLocation :: LocationInfo, _ptA :: a}
	deriving (Show, Ord, Eq, Functor)

makeLenses ''ParseTree'

type ParseTree	= ParseTree' SyntFormIndex

-- Used only here
type Syntaxes	= Map [Name] Syntax


instance Normalizable (ParseTree' a) where
	normalize (Seq [pt] _ _)
		= pt
	normalize (RuleEnter pt nm i loc a)
		= RuleEnter (normalize pt) nm i loc a
	normalize pt =  pt 


deAnnot	:: ParseTree' a -> ParseTree' ()
deAnnot pt
	= pt |> const ()

annot	:: a -> ParseTree' x -> ParseTree' a
annot a pt
	= pt |> const a

removeLocationInfo	:: ParseTree' a -> ParseTree' a
removeLocationInfo re@RuleEnter{}
	= re	& set ptLocation unknownLocation
		& over pt removeLocationInfo
removeLocationInfo seq@Seq{}
	= seq	& set ptLocation unknownLocation
		& over (pts . mapped) removeLocationInfo
removeLocationInfo pt
	= pt & set ptLocation unknownLocation


removeRuleEnters	:: ParseTree' a -> ParseTree' a
removeRuleEnters (RuleEnter pt _ _ _ _)
			= removeRuleEnters pt
removeRuleEnters (Seq pts loc a)
			= Seq (pts |> removeRuleEnters) loc a
removeRuleEnters pt	= pt


mostSpecificRuleEnter	:: ParseTree' a -> ParseTree' a
mostSpecificRuleEnter (RuleEnter (re@RuleEnter{}) _ _ _ _)
	= mostSpecificRuleEnter re
mostSpecificRuleEnter (Seq pts loc a)
	= Seq (pts |> mostSpecificRuleEnter) loc a
mostSpecificRuleEnter pt
	= pt

{-  | Removes all superfluous information (location info and such), so that parsetrees can be compared to the bare structure and contents
-}
bareInformation	:: ParseTree' a -> ParseTree' ()
bareInformation pt
	= pt & deAnnot & removeLocationInfo & removeRuleEnters

contents	:: ParseTree' a -> String
contents (Literal token _ _ _)
	= token
contents (Int i _ _)
	= show i
contents (Seq pts _ _)
	= pts >>= contents
contents (RuleEnter pt _ _ _ _)
	= contents pt


isHiddenPt	:: ParseTree' a -> Bool
isHiddenPt (Literal _ _ _ hidden)
	= hidden
isHiddenPt _
	= False

removeHidden	:: ParseTree' a -> ParseTree' a
removeHidden (Seq pts l a)
	= let pts'	= pts & filter (not . isHiddenPt) |> removeHidden in
		Seq pts' l a
removeHidden re@(RuleEnter pt nm i location a)
	= let	pt'	= pt & removeHidden in
		RuleEnter pt' nm i location a
removeHidden pt
	= pt


--------------------- PARSER STUFF --------------------------------------

{- | Converts a string to a simple parsetree

>>> simplePT "abc"
Literal {_ptToken = "abc", _ptLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}, _ptA = (), _ptHidden = False}
-}
simplePT	:: String -> ParseTree' ()
simplePT string	= Literal string unknownLocation () False


{- | Parses a file with the given syntax

-}
parse	:: FilePath -> (Syntaxes, [Name]) -> Name -> String -> Failable ParseTree
parse fileName syntax syntacticForm
	= _runParser fileName (_parseRule syntax syntacticForm <* eof)

_runParser	:: FilePath -> Parser a -> String -> Failable a
_runParser fileName parser string
	= runParser parser (ParserMetaInfo fileName 0 0) fileName string
		& fromParseError


_parseRule	:: (Syntaxes, [Name]) -> Name -> Parser ParseTree
_parseRule (syntaxes, ns) nm
	= do	syntax'	<- checkExistsSugg show ns syntaxes ("No namespace "++intercalate "." ns++" found while attempting to parse "++show nm) & either fail return
		let syntax	= get grouperDict syntax'	
		syntForm	<- checkExistsSugg show nm syntax ("Syntactic form "++showFQ (ns, nm) ++" not found")
					& either fail return
		let choices	= get syntChoices syntForm
		choice (mapi choices |> _parseChoice syntaxes (ns, nm) |> try)

_parseChoice	:: Syntaxes -> FQName -> (Int, BNF) -> Parser ParseTree
_parseChoice syntax nm (i, choice)
	= try $
	  do	let sf	= SyntFormIndex nm i Nothing
		start	<- location
		pt	<- _parseBNF syntax (choice, sf)
		info	<- endToken start
		return $ RuleEnter pt nm i info sf
		


_parseBNF	:: Syntaxes -> (BNF, SyntFormIndex) -> Parser ParseTree
_parseBNF syntax (BNF.Literal str, sf)
		= do	start	<- location
			string str
			skipChars str
			info	<- endToken start
			return $ Literal str info sf False
			
_parseBNF syntax (BNF.BuiltIn hidden builtin, sf)
		= do	start	<- location
			parsed	<- get biParse builtin
			skipChars (either id show parsed)
			info	<- endToken start
			let nm 	= (["ALGT","Builtins"], get BNF.biName builtin) :: FQName
			parsed & either (\str -> Literal str info sf hidden) (\i -> Int i info sf)
				& return
			

_parseBNF syntaxes (BNF.RuleCall (ns, nm), _)
		= _parseRule (syntaxes, ns) nm

_parseBNF syntax (BNF.Group bnf, sf)
		= do	start	<- location
			pt	<- _parseBNF syntax (bnf, sf)
			info	<- endToken start
			return $ Literal (contents pt) info (NoIndex ([], "String")) False


_parseBNF syntax (BNF.Seq bnfs, sf)
		= do	start	<- location
			pts	<- bnfs & mapi |> swap 
					||>> (\i -> set syntIndSeqInd (Just i) sf)
					|+> _parseBNF syntax
			info	<- endToken start
			return $ Seq pts info sf



endToken	:: (Int, Int) -> Parser LocationInfo
endToken (lStart, cStart)
	= do	(lEnd, cEnd)	<- location
		file	<- getState |> get pmiFile
		return $ LocationInfo lStart lEnd cStart cEnd file

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


instance ToString (ParseTree' a) where
	toParsable (Literal str _ _ _)
		= str
	toParsable (Int i _ _)
		= show i
	toParsable (Seq s _ _)
		= s >>= toParsable
	toParsable (RuleEnter pt _ _ _ _)
		= toParsable pt

	toCoParsable (Literal str _ _ _)
		= show str
	toCoParsable (RuleEnter pt _ _ _ _)
		= toCoParsable pt
	toCoParsable pt
		= toParsable pt

	debug (Literal str _ _ _)
		= "(Literal "++show str++")"
	debug (Int i _ _)
		= show i
	debug (Seq pts _ _)
		= pts |> debug & commas & inParens'
	debug (RuleEnter pt name choice _ _)
		= (debug pt & inParens) ++ (showFQ name++":"++show choice)


asTree	(RuleEnter pt usedR usedind _ _)
	= "+ " ++ showFQ usedR ++ ":" ++ show usedind ++ "\n" ++ (asTree pt & indentWith "| ") 
asTree (Seq pts _ _)
	= pts |> asTree & unlines
asTree pt
	= toParsable pt

