module Main where

import Utils.All

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
		args'	<- parseArgs version args
		repl (get replPath args') (get replModule args' & uncalate '.')
