module Main where

import Lib
import Repl

import ArgumentParser

import System.Environment
import System.IO

versionCount	= [0,2,0]
versionMessage	= "Hello World, again"
version		= (versionCount, versionMessage)

main :: IO ()
main = do	args	<- getArgs
		parseArgs version args
		putStrLn "Hello world"
