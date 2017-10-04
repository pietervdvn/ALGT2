{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}
module LanguageDef.Tools.Grouper where

{- A 'Grouper' groups several name-indexed values in a certain order -}

import Utils.All

import Data.Map as M
import Data.List as L

import Control.Arrow ((&&&))

import Lens.Micro (Lens)

data Grouper x	= Grouper
		{ _grouperDict	:: Map Name x
		, _grouperOrder	:: [Name]
		, _groupName	:: (Name, Name)
		} deriving (Show, Ord, Eq)
makeLenses ''Grouper

emptyGrouper	:: (Name, Name) -> Grouper a
emptyGrouper
	= Grouper M.empty []

asGrouper	:: (Name, Name) -> (a -> Name) -> [a] -> Grouper a
asGrouper groupName getName as
	= Grouper (as |> (getName &&& id) & M.fromList)
			(as |> getName)
			groupName 

overGrouperM	:: Monad m => (a -> m b) -> Grouper a -> m (Grouper b)
overGrouperM
	= overGrouperMCmb sequence

overGrouperMCmb	:: Monad m => ([m (Name, b)] -> m [(Name, b)]) -> (a -> m b) -> Grouper a -> m (Grouper b)
overGrouperMCmb sequence fm (Grouper dict order name)
	=  do	dict'	<- dict & M.toList ||>> fm |> sndEffect & sequence |> M.fromList
		return $ Grouper dict' order name

overGrouperMCmb'	:: Monad m => ([m (Name, b)] -> m [(Name, b)]) -> (a -> m b) -> Maybe (Grouper a) -> m (Maybe (Grouper b))
overGrouperMCmb' _ _ Nothing
		= return Nothing
overGrouperMCmb' cmb f (Just grouper)
		= overGrouperMCmb cmb f grouper |> Just

overGrouperM'	:: Monad m => (a -> m b) -> Maybe (Grouper a) -> m (Maybe (Grouper b))
overGrouperM'	= overGrouperMCmb' sequence



overGrouperLens	:: Monad m => Lens ld ld (Maybe (Grouper x)) (Maybe (Grouper x)) 
				 -> (x -> m x) -> ld -> m ld
overGrouperLens lens fixer langDef
	= do	let grouper	= get lens langDef
		grouper'	<- grouper |> overGrouperM fixer & justEffect
		return $ set lens grouper' langDef



instance ToString a => ToString (Grouper a) where
	toParsable (Grouper as order _)
		= let	order'	= order ++
				(as & M.keys & L.filter (`notElem` order)) in
			order'	|> (as ! )
				|> toParsable
				& unlines


instance Checkable a => Checkable (Grouper a) where
	check (Grouper as order groupName )
		= let	ch1	= _checkNoDups groupName order
			chcks	=  as & M.elems |> check
			in
			allRight_ (ch1:chcks)


instance Checkable' x a => Checkable' x (Grouper a) where
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
