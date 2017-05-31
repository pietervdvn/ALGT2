 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module Utils.ToString where

{-

Haskell has already a built-in show; but we need more. 
We often want a data representation (often handily available through `deriving Show`), but we often want fancy representations, in multiple flavours.

A flavour could be that we need some more data to accuratly show the data, a version for 'native' (e.g. in the target language) or as expression (in the meta-stuff, e.g. proofs)

These are presented here.
-}

import Data.List (intercalate)

class ToString a where
	-- show gives us the default haskell representation


	-- how you would write it in the original data file
	-- e.g. a parsetree as if it was target language, 
	-- a meta-expression as if it came from a typesystem file
	toParsable	:: a -> String

	-- Show as if this were in the opposite file
	-- e.g. a parsetree as if it was a meta-expression in a typesystem file
	-- a meta-expression as if it was target language. This might not always be possible
	toCoParsable	:: a -> String
	toCoParsable	= toParsable
	
	-- can contain more info, e.g. type info of the expression
	debug	:: a -> String
	debug	= toParsable


class ToString' opts a where
	show'		:: opts -> a -> String
	toParsable'	:: opts -> a -> String
	toCoParsable'	:: opts -> a -> String
	debug'		:: opts -> a -> String


instance ToString a => ToString' () a where
	show'		= const toParsable
	toParsable'	= const toParsable
	toCoParsable'	= const toCoParsable
	debug'		= const debug

instance (Show a, ToString a) => ToString' String [a] where
	show' s as		= intercalate s $ map show as
	toParsable' s as	= intercalate s $ map toParsable as
	toCoParsable' s as	= intercalate s $ map toCoParsable as
	debug' s as		= intercalate s $ map debug as



printPars	:: (ToString a) => a -> IO ()
printPars a	= putStrLn $ toParsable a

printPars'	:: (ToString' x a) => x -> a -> IO ()
printPars' x a	= putStrLn $ toParsable' x a


printCoPars	:: (ToString a) => a -> IO ()
printCoPars	= putStrLn . toCoParsable
	


printDebug	:: (ToString a) => a -> IO ()
printDebug	= putStrLn . debug
