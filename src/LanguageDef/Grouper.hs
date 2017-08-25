{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
module LanguageDef.Grouper where

{- A 'Grouper' groups several name-indexed values in a certain order -}

import Utils.All

import Data.Map as M
import Data.List as L

import Control.Arrow ((&&&))

data Grouper x	= Grouper
		{ _grouperDict	:: Map Name x
		, _grouperOrder	:: [Name]
		, _groupName	:: (Name, Name)
		} deriving (Show, Ord, Eq)
makeLenses ''Grouper


asGrouper	:: (Name, Name) -> (a -> Name) -> [a] -> Grouper a
asGrouper groupName getName as
	= Grouper (as |> (getName &&& id) & M.fromList)
			(as |> getName)
			groupName 

overGrouperM	:: Monad m => (a -> m a) -> Grouper a -> m (Grouper a)
overGrouperM fm (Grouper dict order name)
	= do	dict'	<- dict & M.toList ||>> fm |+> sndEffect |> M.fromList
		return $ Grouper dict' order name

instance ToString a => ToString (Grouper a) where
	toParsable (Grouper as order _)
		= let	order'	= order ++
				(as & M.keys & L.filter (`notElem` order)) in
			order'	|> (as ! )
				|> toParsable
				& unlines


instance Check a => Check (Grouper a) where
	check (Grouper as order groupName )
		= let	ch1	= _checkNoDups groupName order
			chcks	=  as & M.elems |> check
			in
			allRight_ (ch1:chcks)


instance Check' x a => Check' x (Grouper a) where
	check' x (Grouper as order groupName)
		= let	ch1	= _checkNoDups groupName order 
			chcks	= as & M.elems |> check' x
			in
			allRight_ (ch1:chcks)

_checkNoDups (name, names) order
	= checkNoDuplicates order (\nms -> if length nms == 1 then unwords ["The", name, show $ head nms, "is defined multiple times"]
					else "Some "++names++" are defined multiple times: "++ (nms |> show & commas))

instance Functor Grouper where
	fmap f (Grouper dict nms groupName)
		= Grouper (dict |> f) nms groupName
