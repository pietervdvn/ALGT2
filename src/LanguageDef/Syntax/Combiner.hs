{-# LANGUAGE TemplateHaskell, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses #-}
module LanguageDef.Syntax.Combiner where


import Utils.All


import LanguageDef.Syntax.Syntax
import LanguageDef.LocationInfo
import LanguageDef.Syntax.BNF hiding (Literal, Seq, string)
import qualified LanguageDef.Syntax.BNF as BNF
import LanguageDef.Syntax.ParseTree

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe 
import Data.Either
import Data.Bifunctor

import Text.Parsec

import Control.Monad

{- A combiner is a tree mimmicing the BNF structure, which converts a parsetree into a user chosen data structure -}
data Combiner a	= LiteralC Doc (String -> Either String a)
		| forall b c . SeqC (Combiner b) (Combiner c) (b -> c -> a)
		| forall b . MapC (Combiner b) (b -> a)
		| IntC (Int -> Either String a)
		| Value a
		| Annot Name [Combiner a]	-- Enter a certain rule, with one combiner for each choice
		| forall b . WithLocation (Combiner b) (LocationInfo -> b -> a)
		| Debug (ParseTree' -> String)			-- Crash and print the parsetree through an error message with the given msg





instance Check' (Syntaxes, [Name]) (Combiner a) where
	check' syntaxes combiner
		= _check syntaxes S.empty combiner


{-
This check checks combiners against the syntax, so that:
- Each choice is covered
- Length of sequences is correct and in headform (Seq _ (Seq _ (Seq _))), where _ is never a Seq (or a fmapped seq)
- Each recursive combiner is correct
- No debugs are found

This does the call check; not that the given rule should be in the current namespace
To prevent loops, already checked is a blacklist
-}
_check	:: (Syntaxes, [Name]) -> Set FQName -> Combiner a -> Either String ()
_check synts ac (MapC c _)
	= _check synts ac c
_check synts _ (Value a)
	= Right ()
_check synts ac (WithLocation cmb _)
	= _check synts ac cmb
_check synts _ (Debug _)
	= Left $ "Debug value found"
_check (synts, ns) ac (Annot rule choices)
 | (ns, rule) `S.member` ac
	= Right ()	-- Already checked
 | otherwise
	= do	synt		<- checkExists ns synts ("No namespace "++ intercalate "." ns)
		choices'	<- checkExistsSugg id rule (synt & get syntax) ("Syntactic form "++show rule++" not found within "++intercalate "." ns)
					||>> fst ||>> removeWS -- Remove injected WS builtin
		let ac'	= S.insert (ns, rule) ac
		let recCheck	= zip choices choices'	|> uncurry (_checkBNF (synts, ac'))
		let choiceIndicator	= (recCheck |> either (const "✘ ") (const "✓ ")) ++ repeat "  "
		let sameLength	= assert (length choices == length choices') $ unlines
			[ "Only "++show (length choices)++" choices provided in combiner for "++rule
			, "Choices that should be accounted for are:\n"++indent (choices' |> toParsable & zipWith (++) choiceIndicator & unlines)
			]
		inMsg ("While checking the combiner for "++showFQ (ns, rule)) $ 
			allRight_ (sameLength:recCheck)
		pass

_check (_, ns) _ cmbr
	= Left $ "Could not check a combiner without top-level annotation element: "++show cmbr


_checkBNF	:: (Syntaxes, Set FQName) -> Combiner a -> BNF -> Either String ()
_checkBNF synts (MapC c _) bnf
	= _checkBNF synts c bnf
_checkBNF synts (Value a) _
	= Right ()
_checkBNF synts (WithLocation cmb _) bnf
	= _checkBNF synts cmb bnf
_checkBNF synts (Debug _) _
	= Left $ "Debug value found"
_checkBNF synts cmb (BNF.Seq [bnf])
	= _checkBNF synts cmb bnf
_checkBNF synts (SeqC cmbb cmbc _) (BNF.Seq (bnf:bnfs))
	= do	_checkBNF synts cmbb bnf
		_checkBNF synts cmbc (BNF.Seq bnfs)
_checkBNF (synts, ac) annot@(Annot{}) (RuleCall (ns, _))
	= _check (synts, ns) ac annot
_checkBNF synts annot@(Annot nm _) bnf
	= Left $ "Could not match "++show (toParsable bnf)++" against the combiner expecting a "++nm
_checkBNF synts (LiteralC _ _) (Group _)
	= Right ()
_checkBNF synts (LiteralC _ _) (BNF.Literal _)
	= Right ()
_checkBNF synts (LiteralC _ _) builtin@(BNF.BuiltIn _ _)
	= Right ()
_checkBNF synts (IntC _) builtin@(BNF.BuiltIn _ bi)
 | get biName bi `elem` ["Number", "Integer"] 
	= Right ()
 | otherwise
	= Left $ "Can not capture a integer/number, use the builtin Number or Integer"

_checkBNF synts comb bnf
	= Left $ "Could not match "++show (toParsable bnf) ++" against combiner "++show comb


instance Show  (Combiner a) where
	show c	= _show 5 c


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
	= pt & removeHidden & _interpret cmb

_interpret	:: Combiner a -> ParseTree' -> Either String a
_interpret (Debug f) pt
	= Left $ f pt
_interpret (LiteralC _ fa) (Literal str' _ _ _)
 	= fa str'
_interpret combiner (Seq [pt] _ _)
	= _interpret combiner pt
_interpret (SeqC ac bc f) sq@(Seq (p:ptss) _ _)
	= do	a	<- _interpret ac p
		b	<- _interpret bc $ set pts ptss sq
		return $ f a b
_interpret (MapC ca f) pt
	= do	a	<- _interpret ca pt
		return $ f a
_interpret (IntC f) (Int i _ _)
	= f i
_interpret (Value a) _
	= return a
_interpret (Annot ruleName cmber) (RuleEnter pt' ruleName' choice _ _)
	= inMsg ("While interpreting a combiner for "++ruleName) $
          do	assert (ruleName == ruleName') $ "Assertion failed: could not interpret, expected construction with "++show ruleName++" but got a "++show ruleName'
		assert (length cmber > choice) $ "Assertion failed: no choice with index "++show choice++" for the given combiner"
		_interpret (cmber !! choice) pt'
_interpret (WithLocation cb flib2a) pt
	= do	let li	= get ptLocation pt
		b	<- _interpret cb pt
		return $ flib2a li b
_interpret combiner parsetree
	= Left $ "Could not interpret "++show combiner++" over "++debug parsetree


capture	:: Combiner String
capture = LiteralC "Capture" Right

int	:: Combiner Int
int	= IntC Right

lit	:: String -> Combiner String
lit str	= LiteralC (show str) (\str' -> if str == str' then Right str else Left $ "Expected literal string "++show str++", but got "++show str')

cmb	:: (b -> c -> a) -> Combiner b -> Combiner c -> Combiner a
cmb f cb cc
	= SeqC cb cc f

err	:: (ParseTree' -> String) -> Combiner ()
err f	= Debug f

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



choices	:: String -> [Combiner a] -> Combiner a
choices = Annot

withLocation	:: (LocationInfo -> a -> b) -> Combiner a -> Combiner b
withLocation f ca
	= WithLocation ca f

withLocation'	:: (a -> LocationInfo -> b) -> Combiner a -> Combiner b
withLocation' f ca
	= WithLocation ca (\li a -> f a li)
