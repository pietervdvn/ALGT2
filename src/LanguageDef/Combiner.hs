{-# LANGUAGE ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses #-}
module LanguageDef.Combiner where


import Utils.All hiding (assert)

import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.Checkable
import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.Grouper

import LanguageDef.Data.BNF hiding (Literal, Seq, string)
import qualified LanguageDef.Data.BNF as BNF
import LanguageDef.Data.SyntacticForm
import LanguageDef.Data.ParseTree

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe 
import Data.Either
import Data.Bifunctor

import Text.Parsec hiding (Error)

import Control.Monad

{- A combiner is a tree mimmicking the BNF structure, which converts a parsetree into a user chosen data structure -}
data Combiner a	= LiteralC Doc (ParseTree -> String -> Failable a)
		| forall b c . SeqC (Combiner b) (Combiner c) (b -> c -> a)
		| forall b . MapC (Combiner b) (b -> a)
		| IntC (ParseTree -> Int -> Failable a)
		| Value a
		| Annot FQName [Combiner a]	-- Enter a certain rule, with one combiner for each choice
		| forall b . WithLocation (Combiner b) (LocationInfo -> b -> a)
		| Debug (ParseTree -> String)			-- Crash and print the parsetree through an error message with the given msg





instance Checkable' Syntaxes (Combiner a) where
	check' syntaxes cmb
		= (inPhase Validating $ _check syntaxes S.empty cmb)


{-
This check checks combiners against the syntax, so that:
- Each choice is covered
- Length of sequences is correct and in headform (Seq _ (Seq _ (Seq _))), where _ is never a Seq (or a fmapped seq)
- Each recursive combiner is correct
- No debugs are found

This does the call check; not that the given rule should be in the current namespace
To prevent loops, already checked is a blacklist
-}
_check	:: Syntaxes -> Set FQName -> Combiner a -> Failable ()
_check synts ac (MapC c _)
	= _check synts ac c
_check synts _ (Value a)
	= pass
_check synts ac (WithLocation cmb _)
	= _check synts ac cmb
_check synts _ (Debug _)
	= fail "Debug value found"
_check synts ac (Annot fqname@(ns, syntForm) choices)
 | (ns, syntForm) `S.member` ac
	= pass	-- Already checked
 | otherwise
	= do	let noNSMsg	= "No namespace found: "++ (if null ns then "(empty namespace)" else show $ dots ns)++" (it should have contained: "++show syntForm
		syntax		<- checkExistsSuggDist' (dots, \nms nms' -> levenshtein (last nms) (last nms') ) ns synts noNSMsg

		let noSFMsg	= "Syntactic form "++show syntForm++" not found within "++intercalate "." ns
		choices'	<- checkExistsSuggDist' (show, levenshtein) syntForm (syntax & get grouperDict) noSFMsg
					|> get syntChoices ||>> removeWS -- Remove injected WS builtin
		let ac'	= S.insert fqname ac
		let recCheck	= zip choices choices'	|> uncurry (_checkBNF (synts, ac'))
		let choiceIndicator	= (recCheck |> handleFailure (const "✘ ") (const "✓ ")) ++ repeat "  "
		let sameLength	= assert' (length choices == length choices') $ unlines
			[ "Only "++show (length choices)++" choices provided in combiner for "++showFQ fqname
			, "Choices that should be accounted for are:\n"++indent (choices' |> toParsable & zipWith (++) choiceIndicator & unlines)
			]
		inMsg' ("While checking the combiner for syntactic form "++showFQ fqname) $ 
			allGood (sameLength:recCheck)
		pass

_check _ _ cmbr
	= fail $ "Could not check a combiner without top-level annotation element: "++show cmbr


_checkBNF	:: (Syntaxes, Set FQName) -> Combiner a -> BNF -> Failable ()
_checkBNF synts (MapC c _) bnf
	= _checkBNF synts c bnf
_checkBNF synts (Value a) _
	= pass
_checkBNF synts (WithLocation cmb _) bnf
	= _checkBNF synts cmb bnf
_checkBNF synts (Debug _) _
	= fail "Debug value found"
_checkBNF synts cmb (BNF.Seq [bnf])
	= _checkBNF synts cmb bnf
_checkBNF synts (SeqC cmbb cmbc _) (BNF.Seq (bnf:bnfs))
	= do	_checkBNF synts cmbb bnf
		_checkBNF synts cmbc (BNF.Seq bnfs)
_checkBNF (synts, ac) annot@Annot{} (RuleCall _)
	= _check synts ac annot
_checkBNF synts annot@(Annot nm _) bnf
	= fail $ "Could not match "++show (toParsable bnf)++" against the combiner expecting a "++showFQ nm
_checkBNF synts (LiteralC _ _) (Group _)
	= pass
_checkBNF synts (LiteralC _ _) (BNF.Literal _)
	= pass
_checkBNF synts (LiteralC _ _) builtin@(BNF.BuiltIn _ _)
	= pass
_checkBNF synts (IntC _) builtin@(BNF.BuiltIn _ bi)
 | get biName bi `elem` ["Number", "Integer"] 
	= pass
 | otherwise
	= fail "Can not capture a integer/number, use the builtin Number or Integer"

_checkBNF synts comb bnf
	= fail $ "Could not match "++show (toParsable bnf) ++" against combiner "++show comb


instance Show  (Combiner a) where
	show	= _show 5


_show	:: Int -> Combiner a -> String
_show 0	_ 
	= " ... "
_show _ (LiteralC doc _)
	= "LiteralC "++inParens doc
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
_show i (Debug _)
	= "DEBUG"

instance Functor Combiner where
	fmap f ca	
		= MapC ca f



interpret cmb pt
	= pt & removeHidden & _interpret cmb & inPhase Parsing & inPtLoc pt

_interpret	:: Combiner a -> ParseTree -> Failable a
_interpret (Debug f) pt
	= fail $ f pt
_interpret (LiteralC _ fa) pt@(Literal str' _ _ _)
 	= fa pt str'
_interpret combiner (Seq [pt] _ _)
	= _interpret combiner pt
_interpret (SeqC ac bc f) sq@(Seq (p:ptss) _ _)
	= do	a	<- _interpret ac p
		b	<- _interpret bc $ set pts ptss sq
		return $ f a b
_interpret (MapC ca f) pt
	= do	a	<- _interpret ca pt
		return $ f a
_interpret (IntC f) pt@(Int i _ _)
	= f pt i
_interpret (Value a) _
	= return a
_interpret (Annot ruleName cmber) (RuleEnter pt' ruleName' choice _ _)
	= inMsg' ("While interpreting a combiner for "++showFQ ruleName) $
          do	assert' (ruleName == ruleName') $ 
			"Expected construction with "++show ruleName++" but got a "++show ruleName'
		assert' (length cmber > choice) $ 
			"No choice with index "++show choice++" for the given combiner"
		_interpret (cmber !! choice) pt'
_interpret (WithLocation cb flib2a) pt
	= do	let li	= get ptLocation pt
		b	<- _interpret cb pt
		return $ flib2a li b
_interpret combiner parsetree
	= fail $ "Could not interpret "++show combiner++" over "++debug parsetree


capture	:: Combiner String
capture = LiteralC "Capture" (const Success)

int	:: Combiner Int
int	= IntC (const Success)

lit	:: String -> Combiner String
lit str	= LiteralC (show str) 
			(\pt str' -> inPhase Parsing $ inPtLoc pt $ 
				do	assert' (str == str') ("Expected literal string "++show str++", but got "++show str')
					return str)

inPtLoc	:: ParseTree' x -> Failable a -> Failable a
inPtLoc pt
	= inLocation (get ptLocation pt)


cmb	:: (b -> c -> a) -> Combiner b -> Combiner c -> Combiner a
cmb f cb cc
	= SeqC cb cc f

err	:: (ParseTree -> String) -> Combiner ()
err	= Debug

skip	:: Combiner ()
skip	= Value ()


skip'	:: a -> Combiner a
skip'	= Value


infixr <**
(<**)	:: Combiner b -> Combiner c -> Combiner b
(<**)	= cmb const

infixr **>
(**>)	:: Combiner b -> Combiner c -> Combiner c
(**>)	= cmb seq

infixr <+>
(<+>)	:: Combiner a -> Combiner b -> Combiner (a, b)
(<+>)	= cmb (,)



choices	:: FQName -> [Combiner a] -> Combiner a
choices = Annot

withLocation	:: (LocationInfo -> a -> b) -> Combiner a -> Combiner b
withLocation f ca
	= WithLocation ca f

withLocation'	:: (a -> LocationInfo -> b) -> Combiner a -> Combiner b
withLocation' f ca
	= WithLocation ca $ flip f
