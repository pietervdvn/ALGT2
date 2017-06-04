module Main where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = test


test :: IO ()
test 	= do	filesSrc	<- glob "src/**/*.hs"
		filesApp	<- glob "app/**/*.hs"
		let args	= ["-fno-warn-tabs"]
		putStrLn "Testing..."
  		doctest (args ++ filesSrc ++ filesApp)
