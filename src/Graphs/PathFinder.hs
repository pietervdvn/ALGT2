module Graphs.PathFinder (searchPath, searchPaths) where

{-
Pathfinder algorithm.
Thanks to @iasoon!
-}

import Utils.Utils
import Data.Maybe
import Data.Map hiding (filter, null)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Function (fix)


-- all paths from start to target. Shortest path first
searchPaths :: (Eq n, Ord n) => Map n (Set n) -> n -> n -> [[n]]
searchPaths links start target =
        let foundPaths  = fix (expandPaths links (endsOn target)) [[start]] in
        fmap reverse foundPaths


searchPath	:: (Eq n, Ord n) => Map n (Set n) -> n -> n -> [[n]]
searchPath links start target =
	let	isValid		= endsOn target
		foundPaths	= until (any isValid <||> null) (>>= expandPath links) [[start]] in
		filter isValid foundPaths |> reverse

endsOn target	= (== target) . head


expandPaths	:: (Eq n, Ord n) => Map n (Set n) -> ([n] -> Bool) -> ([[n]] -> [[n]]) -> [[n]] -> [[n]]
expandPaths _ _ _ []	= []
expandPaths links valid recur start
		= let	expanded	= start >>= expandPath links in
			filter valid expanded ++ recur expanded

expandPath 	:: (Eq n, Ord n) => Map n (Set n) -> [n] -> [[n]]
expandPath links path	=
        let	foundLinks      =  getLinks (head path) links
            	nonCyclicLinks  =  S.filter (not . (`elem` path)) foundLinks in
    		   -- append node to path
            	 S.toList nonCyclicLinks |> (:path)

getLinks 	:: (Eq n, Ord n) => n -> Map n (Set n) -> Set n
getLinks	= findWithDefault S.empty

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) f g v = f v || g v

buildTest	= fmap S.fromList . fromList . merge

test = buildTest [
		('A', 'B'),
		('A', 'C'),
		('A', 'E'),
		('B', 'C'),
		('B', 'D'),
		('C', 'D'),
		('D', 'E'),
		('E', 'F')
		]

test0 = buildTest [('A','B'),
			('B','A')]
