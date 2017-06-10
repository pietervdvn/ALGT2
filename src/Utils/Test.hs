module Utils.Test where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

test :: IO ()
test 	= do	filesSrc	<- glob "src/**/*.hs"
		filesApp	<- glob "app/**/*.hs"
		let args	= ["-fno-warn-tabs"]
		putStrLn "Testing..."
  		doctest (args ++ filesSrc ++ filesApp)


