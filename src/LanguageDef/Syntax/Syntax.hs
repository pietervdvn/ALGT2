{-# LANGUAGE RankNTypes, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
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
import LanguageDef.Syntax.BNF

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



data Syntax = Syntax
		{ _syntax :: Map Name [(BNF, MetaInfo)]
		, _syntaxElementsOrder	:: [(Name, MetaInfo)]	-- Only for pretty printing and error detection
		}
	deriving (Show, Eq)
makeLenses ''Syntax

emptySyntax	= Syntax M.empty []


syntaxChoices
	= syntax . mapped . mapped . _1

-------------------- SYNTAX TOOLS ----------------------

type Syntaxes	= Map [Name] Syntax

asSyntaxes	:: [Name] -> Syntax -> (Map [Name] Syntax, [Name])
asSyntaxes ns s
	= (M.singleton ns s, ns)

asSyntaxes'
	= M.singleton


existingNames	:: Syntaxes -> Set FQName
existingNames s
	= s |> get syntax |> M.keys & M.toList & unmerge & S.fromList


callExists	:: Syntaxes -> FQName -> Bool
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
		_checkNoDuplicate (merged, [])
		return merged

mergeSyntax' a b
	= mergeSyntax a b  & either error id






-------------------------------- CHECKS -------------------------------------


{- | Checks all kind of stuff

>>> import LanguageDef.API
>>> loadAssetLangDef "Faulty" ["TestShadowing"]
Left "While checking for dead clauses in the syntactic form TestShadowing:\n  While checking syntactic form \"dead1\":\n    The choice 'TestShadowing.expr' does prevent a later choice from being parsed, killing 'TestShadowing.bool'\n  While checking syntactic form \"dead2\":\n    The choice 'TestShadowing.expr' does prevent a later choice from being parsed, killing 'TestShadowing.bool Whitespace \";\"'\n  While checking syntactic form \"x\":\n    The choice '\"a\"' does prevent a later choice from being parsed, killing '\"a\" Whitespace \"b\"'\n  While checking syntactic form \"y\":\n    The choice 'TestShadowing.bool' does prevent a later choice from being parsed, killing 'TestShadowing.bool Whitespace TestShadowing.bool'\n  While checking syntactic form \"z\":\n    The choice 'TestShadowing.bool' does prevent a later choice from being parsed, killing 'TestShadowing.bool'\n"

-}
instance Check' ([Name] -> FQName -> FQName -> Bool) Syntaxes where
	check' isSubtypeOf syntaxes
		= do	let syntaxes'	= M.toList syntaxes |> swap
			syntaxes' |> _checkAllIdentsExist syntaxes  & allRight_
			[ syntaxes' |> _checkNoDuplicate
			 , syntaxes' |> _checkNoTrivial
			 ] |> allRight_ & allRight_
			_checkLeftRecursion syntaxes
			-- Cycles in the supertype relationship: check unneeded, prevented by left recursion check
			syntaxes & M.toList |> _checkDeadIn isSubtypeOf & allRight_


_checkDeadIn	:: ([Name] -> FQName -> FQName -> Bool) -> ([Name],  Syntax) -> Either String ()
_checkDeadIn isSubtype (nm, synt)
	=  do	inMsg ("While checking for dead clauses in the syntactic form "++dots nm) $ 
			synt & get syntax & M.toList |> (\(syntForm, choices) -> inMsg ("While checking syntactic form "++show syntForm) $
				_checkDeadIn' (isSubtype nm) (choices |> fst)) & allRight_

_checkDeadIn'		:: (FQName -> FQName -> Bool) -> [BNF] -> Either String ()
_checkDeadIn' _ [_]	= pass
_checkDeadIn' isSubtypeOf (head:choices)
	= let	recursiveCheck	= _checkDeadIn' isSubtypeOf choices
		msg tested	= "The choice '"++toParsable head++"' does prevent a later choice from being parsed, killing '"++toParsable tested++"'"
		checkOne tested	= assert (not $ doesKill isSubtypeOf head tested) $ msg tested
		checkAll	= choices |> checkOne
		in
		allRight_ (recursiveCheck:checkAll)		
			

-- | A syntactic form should not be declared twice
-- >>> import LanguageDef.Syntax
-- >>> _checkNoDuplicate (asSyntaxUnchecked' "Tests" "\nabc ::= Number\nabc ::= Number\n", ["Tests"])
-- Left ...
-- >>> _checkNoDuplicate (asSyntaxUnchecked' "Tests" "\nabc ::= Number\n", ["Tests"])
-- Right ...
_checkNoDuplicate	:: (Syntax, [Name]) -> Either String ()
_checkNoDuplicate (Syntax synts order, nm)
	= inMsg ("While checking the syntax or "++intercalate "." nm) $ 
	  checkNoDuplicates (order |> fst) $ \dupNames ->
	  let 	dupMeta	= dupNames |> (id &&& flip lookup order) |> sndEffect & catMaybes
		dupEntries
			= dupMeta |> (\(nm, meta) -> nm ++ "\t "++toCoParsable meta) & unlines
		in
		"Some syntactic forms are declared multiple times:" ++ indent dupEntries
		

		


-- | All rulecalls shoud exist
-- >>> import LanguageDef.Syntax
-- >>> let syntax1 = asSyntaxUnchecked' "Tests" "\nabc ::= x\n"
-- >>> _checkAllIdentsExist (asSyntaxes' ["Tests"] syntax1) (syntax1, ["Tests"])
-- Left ...
-- >>> let syntax2 = asSyntaxUnchecked' "Tests" "\nabc ::= abc\n"
-- >>> _checkAllIdentsExist (asSyntaxes' ["Tests"] syntax2) (syntax2, ["Tests"])
-- Right ...
_checkAllIdentsExist	:: Syntaxes -> (Syntax, [Name]) -> Either String ()
_checkAllIdentsExist otherSyntaxes (syntx, localScope)
	= let	existingFQ	= existingNames otherSyntaxes
		called		= syntx & syntaxRuleCalls localScope
					:: [((MetaInfo, Name), FQName)]
		missing	= called & filter ((`S.notMember` existingFQ) . snd)
		printMissing ((mi, nm), fq)
			= showFQ fq ++"\t (used in the declaration of "++nm++", "++toCoParsable (get miLoc mi) ++")"
		in
		assert (null missing) $ 
			"Some used syntactic forms are not known: "++
			(missing |> printMissing |> indent & unlines)
		

-- | Rules are not trivial (= one choice with only the rulecall)
-- >>> import LanguageDef.Syntax
-- >>> let syntax2 = asSyntaxUnchecked' "Tests" "\nabc ::= abc\n"
-- >>> _checkNoTrivial (syntax2, ["Tests"])
-- Left ...
_checkNoTrivial	:: (Syntax, [Name]) -> Either String ()
_checkNoTrivial (synt, nm)
	= let	trivial	= synt & get syntax & M.toList 
				& filter ((==) 1 . length . snd)
				& unmerge	-- does not have a real effect, all the remaining lists have length 1
				& filter (isRuleCall . fst . snd)	-- Now only trivial stuff remains
		printTriv (nm, (bnf, mi))
			= nm++" = "++toParsable bnf++"\t (declared "++toCoParsable (get miLoc mi)++")"
		in
		inMsg ("In the syntax of "++intercalate "." nm ) $
		assert (null trivial) $
			"Some syntactic forms are trivial, remove them: "++
				trivial |> printTriv |> indent & unlines



-- | No left recursion exists
-- >>> import LanguageDef.Syntax
-- >>> let synt0 = asSyntaxUnchecked' "test" "\nabc ::= def\ndef ::= abc\n"
-- >>> _checkLeftRecursion (asSyntaxes' ["test"] synt0)
-- Left ...
-- >>> let synt1 = asSyntaxUnchecked' "test" "\nx ::= x\n"
-- >>> _checkLeftRecursion (asSyntaxes' ["test"] synt1)
-- Left ...
-- >>> let synt2 = asSyntaxUnchecked' "test" "\nx ::= x \"a\"\n"
-- >>> _checkLeftRecursion (asSyntaxes' ["test"] synt2)
-- Left ...
_checkLeftRecursion	:: Syntaxes -> Either String ()
_checkLeftRecursion syntaxes
  = do	let lr	= leftRecursiveCalls syntaxes
			& existingNames & S.toList
	-- only the left recursive stuff is left by the algo
	assert (null lr) $ "Some left recursive rules are left: "
		++ (lr |> showFQ |> indent & unlines) 


-- | Calculates the left recursive calls in all syntaxes
-- Only values left will be left recursive
-- >>> import LanguageDef.Syntax
-- >>> let synt = asSyntaxUnchecked' "Test" "\nabc ::= def\ndef ::= abc\n" & asSyntaxes' ["Test"]
-- >>> leftRecursiveCalls synt & M.toList & head & snd & toParsable
-- "\n\nabc\t::= Test.def\n\n\ndef\t::= Test.abc\n"
leftRecursiveCalls	:: Syntaxes -> Syntaxes
leftRecursiveCalls syntaxes
 = let  -- Remove all but the first element of all choices
	syntaxes' = syntaxes |> over syntaxChoices removeTail
	syntaxes'' = syntaxes'
			-- only keep choices that start with a rulecall
			  |> over (syntax . mapped) (L.filter (isRuleCall . fst))

	fqCall bnf
		= bnf & getRuleCall & fromJust

	-- remove syntactic forms with no resting choices from the syntaxes
	cleanSynt syntaxes
		= syntaxes |> over syntax (M.filter (not . L.null))
	-- remove rules for which the syntactic forms don't exist anymore
	removeEmpty syntaxes
		= syntaxes |> over (syntax . mapped) (L.filter (callExists syntaxes . fqCall . fst))

	in
	fixLoop syntaxes'' (cleanSynt . removeEmpty)



-------------------------------- TOOLS -------------------------------------



-- | All Fully Qualified calls, with the first param the namespace for local calls. Metainfo is the choice in which the call is made, name is the declaration for which the call is made
syntaxRuleCalls	:: [Name] -> Syntax -> [((MetaInfo, Name), FQName)]
syntaxRuleCalls localScope s
	= let	synt	= s & get syntax	:: Map Name [(BNF, MetaInfo)]
		in
		synt & M.toList & unmerge
			|> (\(name, (bnf, mi)) -> ((mi, name), L.nub $ getRuleCalls bnf))
			& unmerge



-------------------------------- UTILS -------------------------------------



instance Normalizable Syntax where
	normalize synt
		= synt & over (syntax . mapped . mapped . _1) normalize







instance ToString Syntax where
	toParsable (Syntax syntax order)
		= order	|> ((fst &&& (flip M.lookup syntax . fst)) &&& snd)
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


