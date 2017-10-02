 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module Utils.Utils where


{-
Some utility functions, often used to program 'left to right'
-}

import Control.Monad

import Data.Either
import Data.Foldable
import Data.Maybe
import Data.List (intercalate, isPrefixOf)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (toUpper)

import Data.Set (Set)
import qualified Data.Set as Set

import Lens.Micro.Extras as LME

import System.Random

import Control.Arrow (first)

get	= LME.view


genRandoms		:: (RandomGen random) => random -> [random]
genRandoms r	= let	(r0, r1)	= split r
			in 
			r0:genRandoms r1

-------------------- usefull types -------------------------------------------------------


type Name = String

class Named n where
	name	:: n -> Name

-- Represents some path in some tree
type Path	= [Int]

-- Markdown documentation
type Doc	= String

type Check	= Either String ()

(<>)	:: Check -> Check -> Check
(<>) a b
	= allRight_ [a, b]

class (Named d) => Documented d where
	documentation	:: d -> Doc

class Checkable a where
	check	:: a -> Check
	check a	= return ()

class Checkable' info a where
	check'	:: info -> a -> Check
	check' info a
		= return ()


checkM	:: Checkable a => Maybe a -> Check
checkM	= maybe pass check


checkM'	:: Checkable' x a => x -> Maybe a -> Check
checkM'	x
	= maybe pass (check' x)

class Normalizable a where
	normalize	:: a -> a

------------------- Left to Right programming helpers -----------------------------------

(&)	= flip ($)

(|>)	:: Functor f => f a -> (a -> b) -> f b
(|>) 	= flip fmap

(|>>=)	:: Monad m => m a -> (a -> m b) -> m b
(|>>=)	= (>>=)

(||>>)	:: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(||>>) container f
	= container |> (|> f) 

(|||>>>) container f
	= container ||>> (|> f)

(|+>)	:: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)
(|+>)	= forM


(|++>)	:: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
(|++>) mta f
	= do	ta	<- mta
		ta |+> f

inParens str	= "("++str++")"
inParens' str	= "["++str++"]"


inDQ str	= "\""++str++"\""

inHeader prefix str chr msg
	= let	title	= " "++str++" "
		line	= title |> const chr
		in
		unlines $ if msg == "" then ["", prefix ++ title, prefix ++ line] 
				else ["", prefix ++ title, prefix ++ line, "", msg]



inHeader' s
	= inHeader "" s '='



showComma	:: Show a => [a] -> String
showComma as	= as |> show & commas


commas		= intercalate ", "
commas' _ [value]
		= value
commas' lastSep stuff
		= (init stuff & commas) ++ " " ++ lastSep ++ " " ++ last stuff


dots		= intercalate "."


allCombinations	:: [[a]] -> [[a]]
allCombinations []	= [[]]
allCombinations (a:as)
	= do	tails	<- allCombinations as
		head	<- a
		return (head:tails)


fixLoop		:: Eq a => a -> (a -> a) -> a
fixLoop a f
	= let	a'	= f a in
		if a' == a then a else fixLoop a' f


------------------- Maybe, Either and other Monad helpers ------------------------------------

pass		:: Monad m => m ()
pass		= return ()

distrEffect	:: Monad m => m (m a, m b) -> (m a, m b)
distrEffect mmamb
		= (mmamb |> fst & join, mmamb |> snd & join)
			

sndEffect	:: Monad m => (a, m b) -> m (a, b)
sndEffect (a, mb)
		= do	b <- mb
			return (a, b)

fstEffect	:: Monad m => (m a, b) -> m (a, b)
fstEffect (ma, b)
		= do	a <- ma
			return (a, b)
ifJust'		:: (a -> b -> IO ()) -> Maybe a -> b -> IO ()
ifJust' f Nothing b	= return ()
ifJust' f (Just a) b	= f a b


justEffect	:: (Monad m) => Maybe (m a) -> m (Maybe a)
justEffect Nothing	= return Nothing
justEffect (Just ma)	= ma |> Just


ifJust		:: (a -> IO ()) -> Maybe a -> IO ()
ifJust _ Nothing	= return ()
ifJust f (Just a)	= f a

firstJusts	:: [Maybe a] -> Maybe a
firstJusts maybes
	= let	as	= catMaybes maybes in
		if null as then Nothing else Just $ head as


-- Gives the first right value. If none of the values is Right, concats the 'error messages' (with newlines and indentation)
firstRight	:: [Either String b] -> Either String b
firstRight vals	= let r	= rights vals in
			if null r then 
				Left (pack $ lefts vals) else 
				Right (head r)
		  where pack vals	= vals |> lines ||>> ("  "++) |> unlines & unlines


-- Checks wether all are right, and returns those. Gives messages for failed values
allRight	:: Show b => [Either String b] -> Either String [b]
allRight eithers
 | all isRight eithers	= Right $ rights eithers
 | otherwise	= eithers |> either id ((++) "Successfull: " . show) & intercalate "\n" & Left


allRight_	:: [Either String ()] -> Either String ()
allRight_ eithers
		= eithers & filter isLeft & allRight >> return ()

allRight'	:: [Either String b] -> Either String [b]
allRight' eithers
		= do	let failed = lefts eithers
			unless (null failed) $ Left $ unlines failed
			return $ rights eithers


indent		= indentWith "  "

indentWith _ []	= []
indentWith str msg	
		= msg & lines |> (str++) & unlines & init

-- Stack message for Either Monad
inMsg		:: String -> Either String a -> Either String a
inMsg msg (Left msg')
		= Left (msg ++ ":\n"++ indent msg')
inMsg _ right	= right


ammendMsg	:: (e -> e) -> Either e a -> Either e a
ammendMsg f (Left e)
		= Left $ f e
ammendMsg _ a	= a


assert :: Bool -> String -> Either String ()
assert False msg	= Left msg
assert True _ 		= pass

----------------------- Tuple Tools ------------------

fst3		:: (a, b, c) -> a
fst3 (a, _, _)	= a

snd3		:: (a, b, c) -> b
snd3 (_, b, _)	= b

trd3		:: (a, b, c) -> c
trd3 (_, _, c)	= c

uncurry3	:: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c)
		= f a b c


dropFst3		:: (a, b, c) -> (b, c)
dropFst3 (_, b, c)	= (b, c)

dropSnd3		:: (a, b, c) -> (a, c)
dropSnd3 (a, _, c)	= (a, c)

dropTrd3	:: (a, b, c) -> (a, b)
dropTrd3 (a, b, _)	= (a, b)

merge3l			:: (a, b, c) -> ((a, b), c)
merge3l (a, b, c)	=  ((a, b), c)

merge3r			:: (a, b, c) -> (a, (b, c))
merge3r (a, b, c)	=  (a, (b, c))


unmerge3r		:: (a, (b, c)) -> (a, b, c)
unmerge3r (a, (b, c))	= (a, b, c) 

unmerge3l		:: ((a, b), c) -> (a, b, c)
unmerge3l ((a, b), c)	= (a, b, c) 


mapBoth f (a, a')	= (f a, f a')
onBoth			= mapBoth

both f (a, a')		= f a && f a'

swap (a, b)		= (b, a)

----------------------- List tools --------------------

chain		:: (Traversable t) => t (a -> a) -> a -> a
chain fs a	= foldl (&) a fs 

validLines	:: String -> [String]
validLines fileConts
		= fileConts & lines & filter (not . null) & filter (not . ("#" `isPrefixOf`))

replaceN	:: Int -> a -> [a] -> [a]
replaceN 0 a (_:as)
		= a : as
replaceN i _ []	= error $ "Replace at index N: index to large. Overshoot: "++show i
replaceN i a (h:as)
		= h : replaceN (i-1) a as



camelCase	:: String -> String
camelCase (' ':c:str)
		= ' ':toUpper c:str
camelCase (c:str)
		= c : camelCase str
camelCase []	= []

uppercase	:: String -> String
uppercase (a:str)
		= toUpper a:str
uppercase []	= []


-- Replaces each element of origs by a choice of pointwise (from a respective column. If a pointwise is empty, the original element is returned
replacePointwise	:: [a] -> [[a]] -> [[a]]
replacePointwise origs pointwise
 | length origs /= length pointwise
	= error $ "Replace pointwise: lengths don't match, got "++show (length origs)++" original elements and "++show (length pointwise)++" lists of choices to choose from"
 | otherwise
	= do	i	<- [0..length origs -1]
		choice	<- pointwise & safeIndex "Pointswise: origs has length " i	
		return $ replaceN i choice origs



length'		:: Int -> String -> Int
length' runningLength []
		= runningLength
length' runningLength ('\t':rest)
		= length' (runningLength & (+ 8) & (`div` 8) & (* 8) ) rest
length' runningLength (a:as)
		= length' (runningLength + 1) as

merge		:: Eq a => [(a,b)] -> [(a,[b])]
merge []	= []
merge ((a,b):ls)
		= let bs	= map snd $ filter ((==) a . fst) ls in
			(a,b:bs): merge (filter ((/=) a . fst) ls)
merge'		:: Eq b => [(a,b)] -> [([a],b)]
merge'		= map swap . merge . map swap

unmerge		:: [(a,[b])] -> [(a,b)]
unmerge 	=  concatMap (\(a,bs) -> [(a,b) | b <- bs])

dubbles		:: Eq a => [a] -> [a]
dubbles []	=  []
dubbles (a:as)	=  (if a `elem` as then (a:) else id) $ dubbles as


checkNoDuplicates	:: (Eq a) => [a] -> ([a] -> String) -> Either String ()
checkNoDuplicates as msg
	= do	let dups	= dubbles as
		unless (null dups) $ Left $ msg dups 


flattenWith		:: (Ord k, Ord k0, Ord k1) => Map k0 (Map k1 v) -> (k0 -> k1 -> k) -> Map k v
flattenWith dict f
	= let	newList = (dict |> Map.toList & Map.toList)
				>>= (\(k0, dict) -> dict |> first (f k0))
		in
		newList & Map.fromList


checkNoCommon		:: (Ord k) => Map k v -> Map k v -> ([(k, (v, v))] -> String) -> Either String ()
checkNoCommon dict1 dict2 errMsg
	= do	let common	= Map.intersection dict1 dict2 & Map.keys
		let v1s		= common |> (dict1 Map.!)
		let v2s		= common |> (dict2 Map.!)
		let msg		= errMsg $ zip common $ zip v1s v2s
		unless (null common) $ Left msg

checkNoExists		:: (Ord k) => k -> Map k v -> String -> Either String ()
checkNoExists k dict msg
 | k `Map.notMember` dict	= return ()
 | otherwise		= Left msg

checkExists		:: (Ord k) => k -> Map k v -> String -> Either String v
checkExists k dict msg
 | k `Map.member` dict
			= return $ dict Map.! k
 | otherwise		= Left msg


checkExistsSugg		:: (Ord k) => (k -> String) -> k -> Map k v -> String -> Either String v
checkExistsSugg show k dict msg
	= do	let available	= dict & Map.keys |> show & intercalate ", "
		checkExists k dict (msg ++ "\nAvailable options are " ++ available)
		

checkAllExists		:: (Ord k) => [k] -> Map k v -> (k -> String) -> Either String [v]
checkAllExists ks dict msg
	= ks |+> (\k -> checkExists k dict (msg k))

padL		:: Int -> a -> [a] -> [a]
padL i filler as
		= replicate (i - length as) filler ++ as

padR		:: Int -> a -> [a] -> [a]
padR i filler as
		= as ++ replicate (i - length as) filler

padR'		:: (Monoid a) => Int -> (a -> Int) -> a -> a -> a
padR' i length filler as
		= as `mappend` (replicate (i - length as) filler & mconcat)

equalizeLength	:: a -> [[a]] -> [[a]]
equalizeLength a ass
	= let	longest		= ass |> length & maximum
		append as	= as ++ replicate (longest - length as) a
		in
		ass |> append

stitch		:: a -> [[a]] -> [[a]] -> [[a]]
stitch eq a b	= let	la	= length a
			lb	= length b
			longest	= max la lb
			a'	= replicate (longest - la) [] ++ a
			b'	= replicate (longest - lb) [] ++ b in
			zipWith (++) (equalizeLength eq a') b'



mapi	:: [a] -> [(Int, a)]
mapi	= zip [0..]


invertDict	:: (Eq b, Ord b, Ord a) => Map a (Set b) -> Map b (Set a)
invertDict	= fmap Set.fromList . Map.fromList . merge . map swap . unmerge . Map.toList . fmap Set.toList

invertDict'	:: (Ord a, Ord b, Eq b) => Map a b -> Map b [a]
invertDict'	=  Map.fromList .  merge . map swap . Map.toList

specialChars	= "+*?[]\\^$.()"

translateRegex	:: String -> String
translateRegex str
	= str >>= (\c -> if c `elem` specialChars then "\\"++[c] else [c])




bar		:: Bool -> Int -> Int -> String -> Int -> String
bar withCounter width total msg' current
 | total < current
	= bar withCounter width current msg' total
 | otherwise
	= let 	current'= fromIntegral current	:: Float
		total'	= fromIntegral total	:: Float
		width'	= fromIntegral width	:: Float
		perc'	= (current' / total') * (width' - 2)
		perc	= round perc'
		max	= show total
		counter	= " (" ++ padL (length max) ' ' (show current) ++ "/" ++ max ++ ") "
		msg	= "--"++take (width - 2) ( if withCounter then counter++ msg' else msg')
		preMsg	= take perc msg
		postMsg	= drop perc msg
		bars	= take perc $ preMsg ++ repeat '-'
		conts	= bars++"█"++postMsg++repeat ' '	in
		"["++ take (width-2) conts ++"]"


{- Ugly hack. 

Consider a list of values [a], with associated message.
An a takes some time to evaluate.
We'd like to show a fancy bar, showing progress.

Solution: 
let msg = fancyString"endMessage" [(someA, "Calculating first a"), (someOtherA, "Calculationg second a")]
putStrLn msg

As fancyString "endMessage" [(someA, "Calculating first a"), (someOtherA, "Calculationg second a")]
evaluates to (as, "[0%   ]\r[--50%  ]\rendMessage", we'll get a fancy bar!)


-}
fancyString	:: String -> [a] -> [String] -> String
fancyString endMsg [] _
	= "\r" ++ endMsg
fancyString endMsg as []
	= fancyString endMsg as [""]
fancyString endMsg (a:as) (msg:msgs)
	= let	msg'	= seq a msg
		restMsg	= fancyString endMsg as msgs
		in
		"\r" ++ msg'++ restMsg


fancyString'	:: Bool -> String -> [a] -> [String] -> String
fancyString' withCounter endMsg as msgs
	= let	l	= length as
		msgs'	= padR l "" msgs 
		prepTuple (i, msg)
			= bar withCounter 80 l msg i
		endMsg'	= padR 80 ' ' endMsg
		in
		fancyString endMsg' as (mapi msgs' |> prepTuple)


todo		= error "TODO"

safeIndex	:: String -> Int -> [a] -> a
safeIndex msg i as
 | i < length as
		= as !! i	-- When grepping on !!; this safe-index... Don't panic :p; Safe !!
 | otherwise	= error $ "Safe index: index to large: "++show i++"("++show (length as)++" available elements) , msg: "++msg


showIdent	:: (Maybe Name, Name) -> String
showIdent (Nothing, nm)
		= nm
showIdent (Just ns, nm)
		= ns ++ "." ++ nm

showIdent'	:: (Name, Name) -> String
showIdent' (ns, nm)
		= ns ++ "." ++ nm


perTwo		:: (a -> a -> a) -> [a] -> [a]
perTwo f (a:b:rest)
		= f a b:perTwo f rest
perTwo _ [a]	= [a]
perTwo _ []	= []



-- calculate levenshtein distance between two strings
levenshtein::[Char] -> [Char] -> Int
-- this part is mostly a speed optimiziation
levenshtein s1 s2
  | length s1 > length s2 = levenshtein s2 s1
  | length s1 < length s2 =
    let d = length s2 - length s1
    in d + levenshtein s1 (take (length s2 - d) s2)
-- the meat of the algorithm
levenshtein "" "" = 0
levenshtein s1 s2
  | last s1 == last s2 = levenshtein (init s1) (init s2)
  | otherwise = minimum [1 + levenshtein (init s1) s2,
                         1 + levenshtein s1 (init s2),
                         1 + levenshtein (init s1) (init s2)]

