{-# LANGUAGE TemplateHaskell #-}
module Graphs.Lattice (Lattice, bottom, top, makeLattice, addElement, addRelation,
			subsetsOf, supersetsOf, allSubsetsOf, allSupersetsOf, isSubsetOf,
			infimum, infimums, supremum, supremums, removeTransitive', removeTransitiveNoCycles,
			debugLattice, emptyLattice) where

{-
This module defines a finite lattice structure.

It allows easy lookup of direct suprema/infima and a chaining

-}

import Utils.Utils

import Lens.Micro hiding ((&), both)
import Lens.Micro.TH

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Graphs.SearchCycles

import Data.Maybe

import Control.Monad
import Control.Monad.State (State, execState)
import qualified Control.Monad.State as State


data Lattice a	= Lattice
	{ _bottom	:: a
	, _top		:: a
	, _isSubsetOfEvery	:: Map a (Set a)	-- every 'a' is also every element of the corresponding set	
	, _isSupersetOfEvery	:: Map a (Set a)
	} deriving (Show, Eq)

makeLenses ''Lattice


emptyLattice t b
	= Lattice t b M.empty M.empty

makeLattice	:: (Show a, Ord a) => a -> a -> Map a (Set a) -> Either [[a]] (Lattice a, [(a, a)])
makeLattice bottom top isSubsetOf'
	= let	elements	= M.keys isSubsetOf' ++ (M.elems isSubsetOf' >>= S.toList)
		restToBottom	= M.singleton bottom (S.fromList elements)
		topToRest	= zip elements (repeat $ S.singleton top) & M.fromList
		isSubsetOf	= M.unions [isSubsetOf' |> S.insert top,
									-- Original set; keep as intact as possible
					M.singleton top S.empty,	-- We add the top element
					restToBottom,			-- We add the bottom element
					topToRest]	-- add top to elements with no subtypes
		
		in do
		(lattice, unneeded)	<- removeTransitive $ Lattice bottom top isSubsetOf (error "Lattices: isSupersetOfEvery used in intialization. This is a bug") 
		let lattice'		= set isSupersetOfEvery (invertDict $ get isSubsetOfEvery lattice) lattice
		return (lattice', unneeded)

addElement	:: (Show a, Ord a) => a -> [a] -> [a] -> Lattice a -> Either [[a]] (Lattice a, [(a, a)])
addElement newEl subsOfNew supersOfNew lattice
	= let	cleanse ls symb	= if null ls then [symb] else ls
		subsOfNew'	= cleanse subsOfNew (get bottom lattice)
		supersOfNew'	= cleanse supersOfNew (get top lattice)

		isSubsetOfEvery'= foldr (M.adjust (S.insert newEl)) (get isSubsetOfEvery lattice) subsOfNew'
					& M.insert newEl (S.fromList supersOfNew')
		isSupersetOfEvery'
				= foldr (M.adjust (S.insert newEl)) (get isSupersetOfEvery lattice) supersOfNew'
					& M.insert newEl (S.fromList subsOfNew')
		lattice'	= lattice	& set isSubsetOfEvery isSubsetOfEvery' 
						& set isSupersetOfEvery isSupersetOfEvery'
		in
		removeTransitive lattice'


addRelation	:: (Ord a, Eq a) => a -> a -> Lattice a -> Lattice a
addRelation sub super l
 | sub == super	= l
 | otherwise
	= l	& over isSubsetOfEvery (M.adjust (S.insert super) sub)
		& over isSupersetOfEvery (M.adjust (S.insert sub) super)

{-
 Consider lattice ["bottom" {"a","b"}] ["a", {"top", "b"} ] ["b", {"top"}]
"bottom" can reach "b" via a, so the direct link "bottom" "b" can be removed

Returns removed links together with the cleaned lattice (or Left $ cycles if cycles exists)
-}
removeTransitive	:: (Ord a) => Lattice a -> Either [[a]] (Lattice a, [(a, a)])
removeTransitive lattice
	= do	let cycles	= cleanCycles $ get isSubsetOfEvery lattice
		unless (null cycles) $ Left cycles
		return $ foldl removeUnneedDirectLinksFor (lattice, []) (M.keys $ get isSubsetOfEvery lattice)


removeTransitive'	:: (Ord a) => Lattice a -> Either [[a]] (Lattice a)
removeTransitive' l
	= removeTransitive l |> fst

removeTransitiveNoCycles	:: (Show a, Ord a) => Lattice a -> Lattice a
removeTransitiveNoCycles l
	= either (\cycles -> error $ "This is not a lattice: cycles detected "++show cycles) id $ removeTransitive' l

removeUnneedDirectLinksFor	:: (Ord a) => (Lattice a, [(a, a)]) -> a -> (Lattice a, [(a, a)])
removeUnneedDirectLinksFor (lattice, removed) a
	= let	-- direct links
		direct		= supersetsOf lattice a
		-- All subsets which can not be reached in one step.
		indirect	= direct & S.toList |> allSupersetsOf lattice & S.unions
		-- In a proper lattice, a related value can be reached in one step or inderectly, but not both at the same time
		-- we take the overlap...
		unneeded	= S.intersection direct indirect
		-- ... and remove it
		subsets'	= foldl (\subsets ch -> M.adjust (S.delete ch) a subsets) (get isSubsetOfEvery lattice) unneeded
		removed'	= zip (S.toList unneeded) (repeat a)
		lattice'	= set isSubsetOfEvery subsets' lattice
		in
		(lattice', removed ++ removed')

		
-- Gives all supersets of a, thus all sets including 'a'
allSupersetsOf	:: (Ord a) => Lattice a -> a -> Set a
allSupersetsOf lattice a
	= let	dirSubs	= supersetsOf lattice a
		subs	= dirSubs & S.toList |> allSupersetsOf lattice & S.unions
		in
		S.union dirSubs subs

-- Gives all subsets of a, thus all (named) sets which are a part of set 'a' (without a itself)
allSubsetsOf	:: (Ord a) => Lattice a -> a -> Set a
allSubsetsOf lattice a
	= let	dirSups	= subsetsOf lattice a
		sups	= dirSups & S.toList |> allSubsetsOf lattice & S.unions
		in
		S.union dirSups sups

-- Direct subsets, << , 'bedekkingsgraad', thus all (named) sets which are a part of set 'a' 
subsetsOf		:: (Ord a) => Lattice a -> a -> Set a
subsetsOf lattice a
	= get isSupersetOfEvery lattice & M.findWithDefault S.empty a

isSubsetOf	:: (Ord a) => Lattice a -> a -> a -> Bool
isSubsetOf lattice sub super
	= sub `S.member` allSubsetsOf lattice super

allElems	:: (Ord a) => Lattice a -> [a]
allElems l	= get top l:get bottom l:(get isSubsetOfEvery l & M.keys)


-- Direct supersets, thus all sets of which set 'a' is a part
supersetsOf		:: (Ord a) => Lattice a -> a -> Set a
supersetsOf lattice a
	= get isSubsetOfEvery lattice & M.findWithDefault S.empty a


infimum		:: (Eq a, Ord a, Show a) => Lattice a -> a -> a -> a
infimum l a b
 | a == b				= a
 | a == get bottom l			= a
 | b == get bottom l			= b
 | a `S.member` allSubsetsOf l b	= a
 | b `S.member` allSubsetsOf l a	= b
 | otherwise	= infimums l $ S.intersection (subsetsOf l a) (subsetsOf l b)

infimums	:: (Eq a, Ord a, Foldable t, Show a) => Lattice a -> t a -> a
infimums l as
 | null as	= get top l
 | length as == 1	= minimum as
 | otherwise	= L.foldl1 (infimum l) as 


supremum	:: (Eq a, Ord a, Show a) => Lattice a -> a -> a -> a
supremum l a b
 | a == b				= a
 | a == get top l			= a
 | b == get top l			= b
 | a `S.member` allSupersetsOf l b	= a
 | b `S.member` allSupersetsOf l a	= b
 | otherwise			= supremums l $ S.intersection (supersetsOf l a) (supersetsOf l b)	-- Empty intersections can not occur here, there will always be top

-- a `elem` c, d
-- b `elem` c, d

-- a `elem` b
-- b `elem` c
-- 

supremums	:: (Eq a, Ord a, Foldable t, Show a) => Lattice a -> t a -> a
supremums l as
 | null as		= get bottom l
 | length as == 1	= maximum as
 | otherwise 		= L.foldl1 (supremum l) as 



------------------- CODE TO SHOW A FANCY SVG ---------------------

-- Gives true if only supertype is top and only subtype is bottom
isDisconnected		:: (Ord a) => Lattice a -> a -> Bool
isDisconnected l a
 	= subsetsOf l a == S.singleton (get bottom l) 
		&& supersetsOf l a == S.singleton (get top l)

{-asSVG			:: (Ord a, Show a) => (a -> String) -> (a -> Bool) -> Int -> ColorScheme -> Lattice a -> String
asSVG shw doDash pxW cs lattice
	= let	(groups, conn, dashed)	= flatRepresentation lattice doDash
		l			= lattice
		groups'			= groups |> filter (not . isDisconnected l)
		-- Dashed ones should be the only ones containing these; but we filter anyway just incase the implementation changes
		conn'			= conn 		& filter (both (not . isDisconnected l))
		dashed'			= dashed 	& filter (both (not . isDisconnected l))		
		shwTpl (a, b)		= (shw a, shw b)
		in latticeSVG  pxW cs (groups' ||>> shw, conn' |> shwTpl, dashed' |> shwTpl)-}
		

-- calculates how many steps might be taken from the top
longestPathToTop	:: (Ord a) => Lattice a -> [(a, Int)]
longestPathToTop l	
		= let	startState	= M.singleton (get top l) (0::Int) in
			execState (untilStable $ stepLongest l) startState & M.toList

untilStable	:: (Eq s) => State s () -> State s ()
untilStable st	= do	st0	<- State.get
			st
			st1	<- State.get
			unless (st0 == st1) $ untilStable st

stepLongest	:: (Ord a) => Lattice a -> State (Map a Int) ()
stepLongest l	= allElems l |+> stepElem l & void
				


stepElem	:: (Ord a) => Lattice a -> a -> State (Map a Int) ()
stepElem l a	= do	let supersOfA	= a & allSupersetsOf l & S.toList
			distances	<- State.get
			let distances'	= supersOfA |> (`M.lookup` distances) & catMaybes
			unless (null distances') (State.modify (M.insert a $ 1 + maximum distances'))



flatRepresentation	:: (Ord a, Show a) => Lattice a -> (a -> Bool) -> ([[a]], [(a, a)], [(a, a)])
flatRepresentation lattice dashedLines
	= let	-- Connections between elements
		relations	= get isSubsetOfEvery lattice |> S.toList & M.toList & unmerge
		(dashed, conn)	= L.partition (\(a, b) -> dashedLines a || dashedLines b) relations
		levels		= longestPathToTop lattice |> swap & merge & L.sortOn fst
		in
		(levels |> snd & reverse, conn, dashed)

nextQueue	:: (Ord a) => Lattice a -> (Set a, [a]) -> (Set a, [a])
nextQueue lattice (alreadySeen, queue)
	= let	queue'	= queue |> (\a -> M.findWithDefault S.empty a (get isSubsetOfEvery lattice & invertDict))
				& S.unions 
				& S.filter (`S.notMember` alreadySeen)
		in
		(S.union alreadySeen queue', S.toList queue')


debugLattice	:: (Ord a) => (a -> String) -> Lattice a -> String
debugLattice show l
	= get isSubsetOfEvery l & M.keys |> debugSubsOf show l & concat

debugSubsOf	:: (Ord a) => (a -> String) -> Lattice a -> a -> String
debugSubsOf show l a
	= let	subs	= subsetsOf l a & S.toList
		subs'	= subs |> show |> indent & unlines
		title	= show a ++ " has following subtypes:"
		in
		title ++ if null subs then "<no subs>\n" else subs'

