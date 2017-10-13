{-# LANGUAGE RankNTypes, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module LanguageDef.Data.SyntacticForm where

{- 

The defintion of
 - Syntaxes
 - Location and meta info
 - BNF

 - BNF-builtins
 - The parser type used (ParsecT) + its state

 -}

import Utils.All

import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.Grouper
import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.Checkable

import LanguageDef.Data.BNF

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Map (Map, fromList, toList)
import Data.Set (Set)
import Data.Function (fix)
import Data.Char (isUpper)
import Data.Bifunctor (first)

import Lens.Micro.TH
import Lens.Micro (mapped)
import Control.Arrow ((&&&))

import Graphs.SearchCycles



data SyntacticForm
	= SyntacticForm
		{ _syntName		:: Name
		, _syntChoices		:: [BNF]
		, _syntChoiceMeta	:: [MetaInfo]	-- meta info of a choice
		, _syntMeta		:: MetaInfo	-- meta info of the entire rule
		} deriving (Show, Eq)

makeLenses ''SyntacticForm

instance Infoable SyntacticForm where
	getInfo sf	= AllInfo (get syntName sf) "Syntactic Form" (get syntMeta sf) (toParsable sf)

type Syntax = Grouper SyntacticForm

{-data Syntax = Syntax
		{ _syntax :: Map Name [(BNF, MetaInfo)]
		, _syntaxElementsOrder	:: [(Name, MetaInfo)]	-- Only for pretty printing and error detection
		}
	deriving (Show, Eq)
makeLenses ''Syntax-}

emptySyntax	= emptyGrouper ("syntax", "syntaxes")


-------------------- SYNTAX TOOLS ----------------------
{-

type Syntaxes	= Map [Name] Syntax

asSyntaxes	:: [Name] -> Syntax -> (Map [Name] Syntax, [Name])
asSyntaxes ns s
	= (M.singleton ns s, ns)

asSyntaxes'
	= M.singleton
-}


createSyntax	:: [SyntacticForm] -> Syntax
createSyntax
	= asGrouper ("syntax", "syntaxes") (get syntName)


-------------------------------- CHECKS -------------------------------------



instance Checkable' (FQName -> Failable FQName, FQName -> FQName -> Bool, [Name]) SyntacticForm where
	check' (resolve, isSubtypeOf, fqname) sf
		= allGood 
			[ _checkAllIdentsExist resolve sf
			, _checkNoTrivial sf
			, _checkDeadClauses isSubtypeOf fqname sf] >> pass


_checkAllIdentsExist	:: (FQName -> Failable FQName) -> SyntacticForm -> Check
_checkAllIdentsExist resolve sf
	= inMsg' ("While resolving all calls in "++get syntName sf) $
		do	let allCalls	= sf & get syntChoices >>= getRuleCalls
			let allCalls'	= allCalls & filter (not . isUpper . head . snd)
			allCalls' |> resolve & allGood
			pass

{- | Rules are not trivial (= one choice with only the rulecall)
>>> import LanguageDef.MetaSyntax
>>> let syntax2 = parseSyntax "Tests" "\nabc ::= abc\n" & crash
>>> let sf = syntax2 & get grouperDict & (M.! "abc")
>>> _checkNoTrivial sf & fromFailed & fromJust & toCoParsable
"Error: \n  \8226 The syntactic form abc is trivial\n  \8226 Remove this rule and replace it by Tests.abc"
-}

_checkNoTrivial	:: SyntacticForm -> Check
_checkNoTrivial sf
	= do	let replacedBy	= get syntChoices sf & head & getRuleCall & fromJust
		assertSugg' (not $ isTrivial sf)
			("The syntactic form "++get syntName sf++" is trivial", "Remove this rule and replace it by "++showFQ replacedBy)
	

isTrivial	:: SyntacticForm -> Bool
isTrivial sf
	= let	choices	= get syntChoices sf
		in
		length choices == 1 && isRuleCall (head choices)

{- | Checks for dead clauses
>>> import LanguageDef.API
>>> loadAssetLangDef "Faulty" ["TestShadowing"] & toCoParsable
"| While validating the syntax while validating \n  Error: \n    \8226 In syntactic form TestShadowing.x\n    \"a\" (choice 0) shadows \"a\" \"b\" (choice 1)\n    \8226 Swap the choices\n  Error: \n    \8226 In syntactic form TestShadowing.y\n    TestShadowing.bool (choice 0) shadows TestShadowing.bool TestShadowing.bool (choice 1)\n    \8226 Swap the choices\n  Error: \n    \8226 In syntactic form TestShadowing.z\n    TestShadowing.bool (choice 0) shadows TestShadowing.bool (choice 1)\n    \8226 Swap the choices\n  Error: \n    \8226 In syntactic form TestShadowing.dead1\n    TestShadowing.expr (choice 0) shadows TestShadowing.bool (choice 1)\n    \8226 Swap the choices\n  Error: \n    \8226 In syntactic form TestShadowing.dead2\n    TestShadowing.expr (choice 0) shadows TestShadowing.bool \";\" (choice 1)\n    \8226 Swap the choices"
-}
_checkDeadClauses	:: (FQName -> FQName -> Bool) -> [Name] -> SyntacticForm -> Check
_checkDeadClauses isSubtypeOf fq sf
	= do	let dead	= deadChoices isSubtypeOf sf
		let sBNF (i, bnf)	= (bnf & removeWS & toParsable) ++ " (choice "++show i++")"
		let choicesMsg (a, b)
				= sBNF a++ " shadows "++ sBNF b
		assertSugg' (L.null dead)
			("In syntactic form "++showFQ (fq, get syntName sf) ++"\n"++ (dead |> choicesMsg & unlines & indent), "Swap the choices")


{- | Filters all dead clauses. Maps an fqnname on shadowing clauses (e.g. "x ::= a | a b" will yield {"x", [(0, 1)]} as choice 0 kills 1)
-}
deadClauses	:: (FQName -> FQName -> Bool) -> [Name] -> Syntax -> Map FQName [((Int, BNF), (Int, BNF))]
deadClauses isSubtypeOf fq synt
	= let	dead	= synt & get grouperDict & M.toList |> (\(nm, sf) -> ((fq, nm), deadChoices isSubtypeOf sf)) 
		dead'	= dead & filter (not . null . snd)
		in
		M.fromList dead'


deadChoices	:: (FQName -> FQName -> Bool) -> SyntacticForm -> [((Int, BNF), (Int, BNF))]
deadChoices isSubtypeOf sf
	= let	choices	= get syntChoices sf & mapi
		doesKill' a b	= doesKill isSubtypeOf (snd a) (snd b)	:: Bool 
		in [(ch1, ch2) | ch1 <- choices, ch2 <- choices, fst ch1 < fst ch2, doesKill' ch1 ch2]
		
		



{- | No left recursion exists
>>> import LanguageDef.API
>>> import LanguageDef.LangDefs
>>> import Data.Maybe (fromJust)
>>> import LanguageDef.Data.LanguageDef
>>> let fqname = ["LeftRecursiveSyntax"]
>>> let unit = loadAssetLangDef "Faulty" fqname & crash' & enterScope fqname & crash
>>> let synt = unit & get (ldScope . langSyntax) & fromJust
>>> _checkLeftRecursion fqname synt
Failed (ExceptionInfo {_errMsg = "Left recursive calls detected:\nLeftRecursiveSyntax.b, LeftRecursiveSyntax.a, LeftRecursiveSyntax.b\n\n", _errSeverity = Error, _errSuggestion = Nothing})
-}


_checkLeftRecursion	:: [Name] -> Syntax -> Check
_checkLeftRecursion fq s
	= do	let cycles = leftRecursiveCalls fq s
		assert' (null cycles) $ unlines
			[ "Left recursive calls detected:"
			, cycles ||>> showFQ |> commas & unlines]


{- | Calculates the left recursive calls in all syntaxes. Only values left will be left recursive
>>> import LanguageDef.API
>>> import LanguageDef.LangDefs
>>> import LanguageDef.Data.LanguageDef
>>> let fqname = ["LeftRecursiveSyntax"]
>>> let unit = loadAssetLangDef "Faulty" fqname & crash' & enterScope fqname & crash'
>>> let synt = unit & get (ldScope . langSyntax) & fromJust
>>> leftRecursiveCalls fqname synt ||>> showFQ |> commas
["LeftRecursiveSyntax.b, LeftRecursiveSyntax.a, LeftRecursiveSyntax.b"]
 
-}
leftRecursiveCalls	:: [Name] -> Syntax -> [[FQName]]
leftRecursiveCalls fq syntax
 = let	syntax' = syntax & get grouperDict & M.toList
			||>> get syntChoices |||>>> (getRuleCall . removeTail)
			||>> catMaybes
			|> first ((,) fq)
			||>> S.fromList
			& M.fromList
				:: Map FQName (Set FQName)
	in cleanCycles syntax'








-------------------------------- UTILS -------------------------------------



instance Normalizable SyntacticForm where
	normalize sf
		= sf & over (syntChoices . mapped) normalize



instance ToString SyntacticForm where
	toParsable (SyntacticForm nm choices chMeta meta)
		= let	docStr		= toParsable meta
			chMeta'		= chMeta |> toCoParsable |> ("\t"++)
			choices'	= choices |> removeWS |> toParsable & zipWith (flip (++)) chMeta'
			allChoices	= choices' & intercalate "\n\t  | "
			assgn	= if all (\bnf -> containsHidden bnf || isSingle bnf) choices then "::=" else "~~="
			in
			["", docStr, nm ++ "\t" ++ assgn ++ " " ++ allChoices] & intercalate "\n"


