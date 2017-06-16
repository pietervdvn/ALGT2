module Graphs.SearchCycles (searchCycles, cleanCycles) where

{--
This module implements a cycle searcher. It works as a reference counting garbage collector
--}


import Utils.Utils hiding (get)
import Control.Monad.State
import Data.Map hiding (null, filter, map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Prelude hiding (lookup)

import Graphs.PathFinder


-- Constructs all possible cycles
cleanCycles	:: (Ord n, Eq n) => Map n (Set n) -> [[n]]
cleanCycles dict
	| M.null dict	= []
	| otherwise	= let 	(start, _)	= findMax dict
				cycles		= filter (/= []) $
							cleanCyclesFrom dict start	in
				cycles ++ cleanCycles (delete start dict)

cleanCyclesFrom	:: (Ord n, Eq n) => Map n (Set n) -> n -> [[n]]
cleanCyclesFrom dict n
		= let   starts	= S.toList $ findWithDefault S.empty n dict
			selfCycles	= if n `elem` starts then [n,n] else []	in
			selfCycles : (starts >>= flip (searchPaths dict) n ) |> (n:)

{-
Calculates the cycles in a given import graph, by deleting all leafs.
Args:
- Map of {n --> depends on these}
- Returns a map of resting dependencies
-}
searchCycles	:: (Ord n, Eq n) => Map n (Set n) -> Map n (Set n)
searchCycles	=  execState removeRecursively

removeRecursively	:: (Ord n, Eq n) => State (Map n (Set n)) ()
removeRecursively 	=  do	stabilize_ checkAllNodes
				modify invertDict	-- by reverting the graph and checking again, the nodes depending on a cycle get removed too
				stabilize_ checkAllNodes
				modify invertDict

checkAllNodes	:: (Ord n) => State (Map n (Set n)) ()
checkAllNodes	=  do	all	<- get
			mapM_ checkNode $ keys all

-- Checks dependencies of a node. If no dependencies remain, the node is removed.
checkNode	:: (Ord n) => n -> State (Map n (Set n)) ()
checkNode n	=  do	directDeps	<- gets (findWithDefault S.empty n) |> S.toList
			depCount	<- mapM numberOfDeps directDeps
			let resting	= map snd $ filter ((> 0) . fst ) $ zip depCount directDeps
			modify $ if null resting then 	delete n
						 else 	insert n $ S.fromList resting


numberOfDeps	:: (Ord n) => n -> State (Map n (Set n)) Int
numberOfDeps n 	=  do	depSet	<- gets $ lookup n
			return $ maybe 0 S.size depSet




-- executes the given state change, until the state is stable
stabilize	:: (Eq s) => State s a -> State s [a]
stabilize act	=  do	s	<- get
			a	<- act
			s'	<- get
			if s == s' then	return [a]
				else do	as	<- stabilize act
					return (a:as)

stabilize_	:: (Eq s) => State s a -> State s ()
stabilize_ act	=  do	s	<- get
			act
			s'	<- get
			when (s /= s') $ stabilize_ act
