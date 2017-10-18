module Main where

import Utils.All
import Assets

import Lib
import Repl

import ArgumentParser

import System.Environment
import System.IO

import Data.Char
import Data.List

import Control.Monad

versionCount	= [0,2,0,4]
versionMessage	= "Hi Reddit! Stuff should actually work now..."
version		= (versionCount, versionMessage)

main :: IO ()
main = do	args	<- getArgs
		args'	<- parseArgs version args
		when (get dumpTemplate args') $ do
			writeFile "Template.language" Assets._Resources_Template_language
			putStrLn "Template/Tutorial was written as Template.language in the current directory"
		get replOpts args' & ifJust (\opts -> 
		    do	let fp		= get replPath opts
			let fp'		= if ".language" `isSuffixOf` (fp |> toLower) then take (length fp - 9) fp else fp
			let modul	= get replModule opts & uncalate '.'
			repl fp' modul)
