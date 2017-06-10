module Utils.CreateAssets (createAssets, createAssets', autoCreateAssets, name, dirConts, autoCreateDevAssets) where

{-
This module defines a small tool, creating asset files
-}

import System.Directory
import Data.List
import Data.Bifunctor
import qualified Data.Map as M
import Data.Foldable
import Data.Char
import qualified Data.ByteString as B

import Control.Monad

(|>)		= flip fmap
(|+>)		= forM
(&)		= flip ($)



binaryFormats	= [".pdf", ".png"]


dirConts	:: FilePath -> IO [FilePath]
dirConts fp'
	= do	let fp	= if last fp' == '/' then fp' else fp' ++ "/"
		files	<- getDirectoryContents fp 
		let files'	= files & filter (not . ("." `isPrefixOf`))
					|> (fp ++)
		mode		<- files' |+> doesDirectoryExist	:: IO [Bool]
		let (directories', normalFiles')	
				=  zip files' mode & partition snd 	:: ([(FilePath, Bool)], [(FilePath, Bool)])
		let directories	= directories' |> fst
		let normalFiles	= normalFiles' |> fst
		recursive	<- directories |+> dirConts
		return $ normalFiles ++ concat recursive
		

replacements	= M.fromList [('.', '_'), ('-', '_'), ('/', '_')]


replace c
	| isAlphaNum c	= M.findWithDefault c c replacements
	| otherwise	= '_'

name		:: String -> String -> String
name origDir fp	= let 	repl	= fp |> replace & ("_"++)
			in drop (1 + length origDir) repl


header dev
      = ["module Assets where"
	, ""
	, ""
	, if dev then "import System.IO.Unsafe (unsafePerformIO)" else ""
	, "import qualified Data.ByteString as B"
	, "import qualified Data.ByteString.Builder as B"
	, "import Data.ByteString.Lazy (toStrict)"
	, ""
	, ""
	, "-- Automatically generated"
	, "-- This file contains all assets, loaded via 'unsafePerformIO' or hardcoded as string, not to need IO for assets"
	, ""
	, ""
	] & unlines


isBinary origDir
	= binaryFormats |> (`isSuffixOf` origDir) & or
		
fileLine	:: Bool -> FilePath -> String -> IO String
fileLine dev origDir file
	= do	let name'	=  name origDir file
		let pragma	= if dev then "\n{-# NOINLINE "++name'++" #-}\n" else ""
		let devAssgn'	= if isBinary file then "unsafePerformIO $ B.readFile "++show file
					else "unsafePerformIO $ readFile "++show file
		let devAssgn	= "let str = "++devAssgn'++" in seq str str"
		contents	<- if dev then return devAssgn else
					if isBinary file then  
						fmap (\bs -> "toStrict $ B.toLazyByteString $ B.string8 "++show bs) (B.readFile file)
						else fmap show (readFile file)
		return $ pragma ++ name' ++ "\n\t = "++contents

allAssetLine origDir fp
	= let	key	= fp & drop (1 + length origDir) & show
		val	= name origDir fp
		in
		"(" ++ key ++ ", "++ val ++ ")"

allAssets	:: String -> [FilePath] -> String
allAssets origDir fps
	= let 	body	= fps	& filter (not . isBinary)
				|> allAssetLine origDir
				& intercalate "\n\t\t\t, " 
				& (\s -> "[" ++ s ++ "\n\t\t\t]")
		funcN	= "allAssets = "
		in
		funcN ++ body ++ "\n"

createAssets'	:: Bool -> FilePath -> IO String
createAssets' dev fp
	= do	files		<- dirConts fp
		-- putStrLn $ "Creating assets for:\n"++(files |> ("   "++) & intercalate "\n")
		contents	<- files |+> fileLine dev fp
		let allA	= allAssets fp files
		return $ header dev ++ allA ++ unlines contents

createAssets	:: Bool -> FilePath -> FilePath -> IO ()
createAssets dev fp target
	= do	contents	<- createAssets' dev fp
		writeFile target contents

autoCreateAssets	:: IO ()
autoCreateAssets
	= createAssets False "src/Assets" "src/Assets.hs"


autoCreateDevAssets
	= createAssets True "src/Assets" "src/Assets.hs"

