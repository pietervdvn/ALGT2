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
			let modul	= get replModule opts
			let modul'	= if ".language" `isSuffixOf` modul then take (length modul - 9) modul else modul
			repl fp (uncalate '.' modul'))
