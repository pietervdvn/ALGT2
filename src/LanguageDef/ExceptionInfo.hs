{-# LANGUAGE TemplateHaskell #-}
module LanguageDef.ExceptionInfo where

{- Gives more information about what goes wrong; with pretty colors -}

import Utils.All hiding (Doc, (<>))

import LanguageDef.LocationInfo
import Text.PrettyPrint.ANSI.Leijen hiding (indent)

import Data.Maybe (isJust)
import Data.Map (Map, member, (!), keys)
import Data.List (partition, sortOn, sort, nub)

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

import Control.Arrow ((&&&))

data Phase	= Loading | Parsing | Resolving | Typing | Validating
	deriving (Show, Eq)

data Severity	= Error | Warning
	deriving (Show, Eq)

data ExceptionInfo = ExceptionInfo
	{ _errMsg		:: String
	, _errSeverity		:: Severity
	, _errPhase		:: Phase
	, _errLocation		:: LocationInfo
	, _errSuggestion	:: Maybe String
	} 
	| Embed	{ _errExtraMsg	:: String
		, _errAmendLoc	:: Maybe LocationInfo
		, _errEmbedded	:: ExceptionInfo}
	| Aggregate [ExceptionInfo]
	deriving (Show, Eq)

makeLenses ''ExceptionInfo


fromAggregate	:: ExceptionInfo -> [ExceptionInfo]
fromAggregate (Aggregate exps)	= exps
fromAggregate exp	= [exp]

flatten		:: [ExceptionInfo] -> [ExceptionInfo]
flatten errs
	= errs |> normalize |> fromAggregate & concat

instance ToString Severity where
	toParsable Error	= "Error" & text & red & show
	toParsable Warning	= "Warning" & text & yellow & show

instance Normalizable ExceptionInfo where
	normalize (Embed ctx li e)
			= case normalize e of
				Aggregate errs	-> errs & flatten |> Embed ctx li & Aggregate
				e		-> Embed ctx li e
	normalize (Aggregate errs)
			= case errs & flatten of
				[e]	-> e
				exceps	-> Aggregate exceps
	normalize ei	= ei

instance ToString Phase where
	toParsable phase	= show phase & over _head toLower

{- | 
>>> let ei	= ExceptionInfo "Something went wrong" Warning Resolving (LocationInfo 42 42 0 5 "SomeFile.language") (Just "Fix by x, y or z") 
>>> ei & toParsable
"\ESC[93mWarning\ESC[0m\ESC[32m in \ESC[1m\ESC[92mSomeFile.language\ESC[0;32;1m\ESC[0;32m, line \ESC[1m42\ESC[0;32m\ESC[0m \ESC[32mwhile resolving:\ESC[0m\n  \8226 Something went wrong\n  \8226 Fix by x, y or z"
-}
instance ToString ExceptionInfo where
	toParsable (ExceptionInfo errMsg severity phase location suggestion)
	      =  let 	meta	= severity & toParsable & text <> inLocation location & dullgreen <+> ("while "++toParsable phase++":") & text & dullgreen
			msg	= text $ indent ("• " ++ errMsg)
			sugg	= text $ indent (maybe "" ("• " ++) suggestion)
			in show $ cat ([meta, msg] ++ if isJust suggestion then [sugg] else [])
	toParsable (Aggregate exceps)
		= exceps |> toParsable & intercalate "\n"

	toParsable (Embed extra amendLocation err)
		= err & over errMsg (extra ++) & over errLocation (\li -> fromMaybe li amendLocation) & toParsable

inLocation	:: LocationInfo -> Doc
inLocation location
 | location == unknownLocation	= empty
 | otherwise
	= text " in" <+> get miFile location & text & green & bold <> text ", line" <+> get liStartLine location & int & bold



data Failable a	= Failed ExceptionInfo | Success a

instance Show a => Show (Failable a) where
	show (Success a)	= "Success "++ inParens (show a)
	show (Failed e)		= toParsable e

instance Functor Failable where
	fmap _ (Failed e)	= Failed e
	fmap f (Success a)	= Success (f a)


instance Applicative Failable where
	pure	= Success
	(<*>) (Failed e1) (Failed e2)
		= Failed $ Aggregate [e1, e2]
	(<*>) (Failed e) _
		= Failed e
	(<*>) (Success f) x
		= fmap f x
	

instance Monad Failable where
	return	= pure
	(>>=) (Success a) a2mb
		= a2mb a
	(>>=) (Failed e) _
		= Failed e


legacy	:: Failable a -> Either String a
legacy (Failed e)
	= Left $ toParsable e
legacy (Success a)
	= Right a


allGood	:: [Failable a] -> Failable [a]
allGood []
	= Success []
allGood (x:xs)
	= (x |> (:)) <*> allGood xs


fromParseError	:: Either Parsec.ParseError a -> Failable a
fromParseError (Right x)
		= Success x
fromParseError (Left pe)
	= let	pos	= Parsec.errorPos pe
		file	= pos & Parsec.sourceName
		line	= pos & Parsec.sourceLine
		col	= pos & Parsec.sourceColumn
		li	= LocationInfo line line col col file
		msg	= pe & Parsec.errorMessages & _showMsgs
		in
		ExceptionInfo "" Error Parsing li Nothing & msg & Failed
		

_showMsgs	:: [Parsec.Message] -> ExceptionInfo -> ExceptionInfo
_showMsgs parsecMsgs exc
		= let	(unExp, exp, msgs)	= _sortMsgs parsecMsgs
			prep list	= list & filter (not . null) & sort & nub
			unExp'	= "Did not expect " ++ commas' "or" (prep unExp) ++ " here"
			exp'	= "Expected "++commas' "or" (prep exp) ++" instead"
			msgsConts	= msgs & prep & commas
			msgs'	= if null msgsConts then "" else msgsConts ++ ": "
			in
			exc & set errMsg (msgs' ++ unExp') & set (errSuggestion . mapped) exp'
			



_sortMsgs	:: [Parsec.Message] -> ([String], [String], [String])
_sortMsgs []	= ([],[], [])
_sortMsgs (Parsec.SysUnExpect unExp:rest)
		= _sortMsgs (Parsec.UnExpect unExp:rest)
_sortMsgs (Parsec.UnExpect unExp:rest)
		= let	(unExps, expecteds, msgs)	= _sortMsgs rest
			unExp'	= if null unExp then "empty string" else unExp
			in
			(unExp':unExps, expecteds, msgs)
_sortMsgs (Parsec.Expect exp:rest)
		= let	(unExps, expecteds, msgs)	= _sortMsgs rest in
			(unExps, exp:expecteds, msgs)
_sortMsgs (Parsec.Message msg:rest)
		= let	(unExps, expecteds, msgs)	= _sortMsgs rest in
			(unExps, expecteds, msg:msgs)


_chainMsg msg ""
		= msg ++ "."
_chainMsg msg str
		= msg ++ ", " ++ str


inMsg'		:: String -> Failable a -> Failable a
inMsg' msg (Failed e)
		= Failed $ Embed msg Nothing e
inMsg' _ (Success x)
		= Success x


checkExists'	::  Ord k => k -> Map k v -> String -> Failable v
checkExists' k dict msg
 | member k dict	= return $ dict ! k
 | otherwise	= Failed $ ExceptionInfo msg Error Resolving unknownLocation Nothing


checkExistsSuggDist'	:: Ord k => (k -> String, k -> k -> Int) -> k -> Map k v -> String -> Failable v
checkExistsSuggDist' (show, score) k dict msg
 | member k dict	= return $ dict ! k
 | otherwise
		= let	kys	= dict & keys |> (id &&& score k)
			(zeroScores, otherScores)
				= kys & partition ((==) 0 . snd) |> sortOn snd |> take 5
			suggs	= (zeroScores ++ otherScores) |> fst |> show & commas
			in
			Failed $ ExceptionInfo msg Error Resolving unknownLocation (Just $ "Perhaps you meant :"++ suggs)


checkExistsSugg' show k dict msg
	= checkExistsSuggDist' (show, const . const 0) k dict msg


handleFailure	:: (ExceptionInfo -> b) -> (a -> b) -> Failable a -> b
handleFailure recover _ (Failed e)
	= recover e
handleFailure _ continue (Success a)
	= continue a


crash		:: Failable a -> a
crash (Failed a)
		= error $ "\n"++toParsable a
crash (Success a)
		= a
