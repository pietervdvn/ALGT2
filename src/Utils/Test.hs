module Utils.Test where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

test' :: String -> IO ()
test' str
	= do	filesSrc	<- glob $ "src/**/"++str++".hs"
		let args	= ["-fno-warn-tabs"]
		putStrLn "Testing..."
  		doctest (args ++ filesSrc)


test	:: IO ()
test	= test' "*" 

