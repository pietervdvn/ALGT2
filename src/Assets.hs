module Assets where


import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (toStrict)


-- Automatically generated
-- This file contains all assets, loaded via 'unsafePerformIO' or hardcoded as string, not to need IO for assets


allAssets = [("TestLanguage.language", _TestLanguage_language)
			, ("MetaFunctionSyntax.language", _MetaFunctionSyntax_language)
			]

{-# NOINLINE _TestLanguage_language #-}
_TestLanguage_language
	 = let str = unsafePerformIO $ readFile "src/Assets/TestLanguage.language" in seq str str

{-# NOINLINE _MetaFunctionSyntax_language #-}
_MetaFunctionSyntax_language
	 = let str = unsafePerformIO $ readFile "src/Assets/MetaFunctionSyntax.language" in seq str str
