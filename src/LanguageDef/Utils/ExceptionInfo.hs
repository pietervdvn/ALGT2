{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
module LanguageDef.Utils.ExceptionInfo where

{- Gives more information about what goes wrong; with pretty colors -}

import Utils.All hiding (Doc, (<>))

import LanguageDef.Utils.LocationInfo
import Text.PrettyPrint.ANSI.Leijen hiding (indent)
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI

import Data.Maybe (isJust, catMaybes, maybeToList)
import Data.Map (Map, member, (!), keys, fromList, toList)
import Data.List (partition, sortOn, sort, nub)

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

import Control.Arrow ((&&&))

data Phase	= Loading | Parsing | Resolving | Typing | Validating | Evaluating
	deriving (Show, Eq)

data Severity	= Error | Warning
	deriving (Show, Eq)

data ExceptionInfo = ExceptionInfo
	{ _errMsg		:: String
	, _errSeverity		:: Severity
	, _errSuggestion	:: Maybe String
	} 
	| Within
	{ _errCtxMsg		:: Maybe String
	, _errCtxPhase		:: Maybe Phase
	, _errCtxLocation	:: Maybe LocationInfo
	, _errEmbedded		:: ExceptionInfo
	}
	| Aggregate [ExceptionInfo]
	deriving (Show, Eq)

makeLenses ''ExceptionInfo



instance ToString' String Severity where
	toParsable' extra severity	= severity & toCoParsable' extra & text & colorFor severity & show
	toCoParsable' extra severity	= show severity ++ extra
	debug'				= toCoParsable'
	show' _				= show


colorFor	:: Severity -> ANSI.Doc -> ANSI.Doc
colorFor Error	= red
colorFor Warning = yellow

fromAggregate	:: ExceptionInfo -> [ExceptionInfo]
fromAggregate (Aggregate exps)	= exps
fromAggregate exp	= [exp]


instance Normalizable ExceptionInfo where
	normalize (Within Nothing Nothing Nothing e)
			= normalize e
	normalize (Within a b Nothing (Within Nothing Nothing c e))
		= Within a b c e & normalize
	normalize (Within a Nothing c (Within Nothing b Nothing e))
		= Within a b c e & normalize
	normalize (Within Nothing b c (Within a Nothing Nothing e))
		= Within a b c e & normalize
	normalize (Within msg ph li e)
			= Within msg ph li $ normalize e
	normalize (Aggregate errs)
			= case errs |> normalize |> fromAggregate & concat of
				[e]	-> e
				exceps	-> Aggregate exceps
	normalize ei	= ei

instance ToString Phase where
	toParsable phase	= show phase & over _head toLower

instance ToString ExceptionInfo where
	toParsable e	= case normalize e of
				(Aggregate errs)	-> errs |> _fancy |> show & intercalate "\n\n"
				e'			-> _fancy e' & show
	toCoParsable e	= e & normalize & _fancy & plain & show
	

_fancy	:: ExceptionInfo -> ANSI.Doc
_fancy (ExceptionInfo errMsg severity suggestion)
	      =  let 	meta	= severity & toCoParsable' ": " & text & colorFor severity
			msg	= errMsg      & ("• " ++)  & indent  & text 
			sugg	= suggestion |> ("• " ++) |> indent |> text
			in vsep ([meta, msg] ++ maybeToList sugg)
_fancy (Within (Just msg) phase li err)
	= let 	msg'	= text msg 
				<> maybe empty (text . (" while " ++) . toParsable) phase
				<+> maybe empty (dullgreen . coloredLocation) li
				<$$> _fancy err
		bar	= colorFor (severityOf err) $ text "| "
		in
		bar <> msg'
_fancy (Within Nothing (Just phase) li err)
	= _fancy $ Within (Just $ "While "++toParsable phase) Nothing li err
_fancy (Within Nothing Nothing (Just li) err)
	= _fancy $ Within (Just $ "At " ++ (coloredLocation li & show)) Nothing Nothing err
_fancy (Within Nothing Nothing Nothing err)
	= _fancy err
_fancy (Aggregate exceps)
	= exceps |> _fancy |> ANSI.indent 2 & vsep


coloredLocation		:: LocationInfo -> ANSI.Doc
coloredLocation (LocationInfo _ _ _ _ "")
	= text "at an unspecified location"
coloredLocation li@(LocationInfo _ _ _ _ file)
	= text "in" <+> green (bold $ text file) <+> text "at" <+> coloredLocation' li

coloredLocation'	:: LocationInfo -> ANSI.Doc
coloredLocation' (LocationInfo sl el sc ec file)
 | (-1) `elem` [sl, el, sc, ec]
	= text "unspecified location"
 | sl == el && sc == ec
	= text "line" <+> bold (int sl) <> text ":" <+> int sc
 | sl == el
	= text "line" <+> bold (int sl) <> text ", columns" <+> int sc <+> text "-" <+> int ec
 | otherwise
	= text "lines" <+> bold (int sl) <+> text "-" <+> bold (int el)


severityOf	:: ExceptionInfo -> Severity
severityOf (ExceptionInfo _ sev _)
	= sev
severityOf (Within _ _ _ err)
	= severityOf err

severityOf (Aggregate errs)
	= severityOf $ head errs


data Failable a	= Failed ExceptionInfo | Success a
	deriving (Show)

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
	fail msg
		= Failed $ ExceptionInfo msg Error Nothing

failSugg	:: (String, String) -> Failable a
failSugg (msg, sugg)
	= ExceptionInfo msg Error (Just sugg) & Failed

multiFail	:: [String] -> Failable a
multiFail msgs
	= msgs |> (\msg -> ExceptionInfo msg Error Nothing) & Aggregate & Failed

instance ToString a => ToString (Failable a) where
	toParsable (Success a)	= toParsable a
	toParsable (Failed (Aggregate errs))
				= _showLined toParsable errs
	toParsable (Failed e)	= toParsable e

	toCoParsable (Success a)= toCoParsable a
	toCoParsable (Failed (Aggregate errs))
				= _showLined toCoParsable errs
	toCoParsable (Failed e)	= toCoParsable e
	
	debug (Success a)	= debug a
	debug (Failed (Aggregate errs))
				= _showLined debug errs 
	debug (Failed e)	= debug e


_showLined s errs
	= Aggregate errs & normalize & fromAggregate |> s & intercalate "\n"


fromFailable	:: Failable a -> Maybe a
fromFailable (Success a)	= Just a
fromFailable _			= Nothing


fromFailed	:: Failable a -> Maybe ExceptionInfo
fromFailed (Failed e)	= Just e
fromFailed _		= Nothing

isSuccess	:: Failable a -> Bool
isSuccess (Success _)	= True
isSuccess _ 		= False


legacy	:: Failable a -> Either String a
legacy (Failed e)
	= Left $ toParsable e
legacy (Success a)
	= Right a


allGood	:: [Failable a] -> Failable [a]
allGood	= foldr (\ x -> (<*>) (x |> (:))) (Success [])


allGoodMap	:: (Ord k) => Map k (Failable v) -> Failable (Map k v)
allGoodMap dict
	= dict & toList |> sndEffect & allGood |> fromList 


fromParseError	:: Either Parsec.ParseError a -> Failable a
fromParseError (Right x)
		= Success x
fromParseError (Left pe)
	= let	pos	= Parsec.errorPos pe
		file	= pos & Parsec.sourceName
		line	= pos & Parsec.sourceLine
		col	= pos & Parsec.sourceColumn
		li	= LocationInfo line line col col file
		(msg, sugg)	= pe & Parsec.errorMessages & _showMsgs
		in
		ExceptionInfo msg Error sugg & Failed & inLocation li & inPhase Parsing
		

_showMsgs	:: [Parsec.Message] -> (String, Maybe String)
_showMsgs parsecMsgs
		= let	(unExp, exp, msgs)	= _sortMsgs parsecMsgs
			prep list	= list & filter (not . null) & sort & nub
			unExp'	= "Did not expect " ++ commas' "or" (prep unExp) ++ " here"
			msg	= "Expected "++commas' "or" (prep exp) ++" instead"
			msgsConts	= (msgs & prep & commas) ++ ": "
			sugg	= if null msgs then Nothing else Just msgsConts
			in
			(unExp', sugg)
			



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


assert'		:: Bool -> String -> Failable ()
assert' True _	= Success ()
assert' False msg
		= fail msg

assertSugg'	:: Bool -> (String, String) -> Failable ()
assertSugg' True _
		= Success ()
assertSugg' False (msg, sugg)
		= Failed $ ExceptionInfo msg Error (Just sugg)


inMsg'		:: String -> Failable a -> Failable a
inMsg' msg (Failed e)
		= e & Within (Just msg) Nothing Nothing & Failed
inMsg' _ success
		= success


inPhase		:: Phase -> Failable a -> Failable a
inPhase phase (Failed f)
		= f & Within Nothing (Just phase) Nothing & Failed
inPhase _ (Success a)
		= Success a

inLocation	:: LocationInfo -> Failable a -> Failable a
inLocation li (Failed f)
		= f & Within Nothing Nothing (Just li) & Failed
inLocation _ success
		= success

inContext	:: (String, Phase, LocationInfo) -> Failable a -> Failable a
inContext (msg, ph, li) (Failed f)
		= f & Within (Just msg) (Just ph) (Just li) & Failed
inContext _ success
		= success




checkExists'	::  Ord k => k -> Map k v -> String -> Failable v
checkExists' k dict msg
 | member k dict	= return $ dict ! k
 | otherwise	= Failed $ ExceptionInfo msg Error Nothing


checkExistsSuggDist'	:: Ord k => (k -> String, k -> k -> Int) -> k -> Map k v -> String -> Failable v
checkExistsSuggDist' (show, score) k dict msg
 | member k dict	= return $ dict ! k
 | otherwise
		= let	kys	= dict & keys |> (id &&& score k)
			(zeroScores, otherScores)
				= kys & partition ((==) 0 . snd) |> sortOn snd |> take 5
			suggs	= (zeroScores ++ otherScores) |> fst |> show & commas
			msg	= if null suggs then Nothing else Just $ "Perhaps you meant: "++ suggs
			in
			Failed $ ExceptionInfo msg Error msg


checkExistsSugg' show
	= checkExistsSuggDist' (show, const . const 0)


handleFailure	:: (ExceptionInfo -> b) -> (a -> b) -> Failable a -> b
handleFailure recover _ (Failed e)
	= recover e
handleFailure _ continue (Success a)
	= continue a


successess	:: [Failable a] -> [a]
successess failables
	= failables |> fromFailable & catMaybes


fails		:: [Failable a] -> [ExceptionInfo]
fails failables
	= failables |> fromFailed & catMaybes


firstSuccess	:: [Failable a] -> Failable a
firstSuccess failables
	= let	allFailed	= null $ successess failables in
		if allFailed then fails failables & Aggregate & Failed
			else successess failables & head & Success

crash		:: Failable a -> a
crash (Failed a)
		= error $ "\n"++toParsable a
crash (Success a)
		= a

-- Mainly used in testing
crash'		:: Failable a -> a
crash' (Success a)
		= a
crash' (Failed a)
		= error $ toCoParsable a
