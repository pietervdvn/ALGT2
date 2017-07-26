module Assets where


import System.IO.Unsafe (unsafePerformIO)
import Control.DeepSeq
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (toStrict)



-- Automatically generated
-- This file contains all assets, loaded via 'unsafePerformIO' or hardcoded as string, not to need IO for assets

readFile'		:: FilePath -> IO String
readFile' str = do	h <- openFile str ReadMode
			s <- hGetContents h
			s `deepseq` hClose h
			return s
allAssets	:: [(FilePath, String)]
allAssets	= [("TestLanguageFaulty.language", _TestLanguageFaulty_language)
			, ("TestLanguage.language", _TestLanguage_language)
			, ("MetaFunctionSyntax.language", _MetaFunctionSyntax_language)
			, ("TestInput/LoopingSupertypes.language", _TestInput_LoopingSupertypes_language)
			, ("TestInput/Nested/Z.language", _TestInput_Nested_Z_language)
			, ("TestInput/Nested/X.language", _TestInput_Nested_X_language)
			, ("TestInput/Nested/L.language", _TestInput_Nested_L_language)
			, ("TestInput/Nested/Y.language", _TestInput_Nested_Y_language)
			, ("Faulty/FunctionTyperTest.language", _Faulty_FunctionTyperTest_language)
			]

{-# NOINLINE _TestLanguageFaulty_language #-}
_TestLanguageFaulty_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestLanguageFaulty.language" in seq str str

{-# NOINLINE _TestLanguage_language #-}
_TestLanguage_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestLanguage.language" in seq str str

{-# NOINLINE _MetaFunctionSyntax_language #-}
_MetaFunctionSyntax_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/MetaFunctionSyntax.language" in seq str str

{-# NOINLINE _TestInput_LoopingSupertypes_language #-}
_TestInput_LoopingSupertypes_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestInput/LoopingSupertypes.language" in seq str str

{-# NOINLINE _TestInput_Nested_Z_language #-}
_TestInput_Nested_Z_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestInput/Nested/Z.language" in seq str str

{-# NOINLINE _TestInput_Nested_X_language #-}
_TestInput_Nested_X_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestInput/Nested/X.language" in seq str str

{-# NOINLINE _TestInput_Nested_L_language #-}
_TestInput_Nested_L_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestInput/Nested/L.language" in seq str str

{-# NOINLINE _TestInput_Nested_Y_language #-}
_TestInput_Nested_Y_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestInput/Nested/Y.language" in seq str str

{-# NOINLINE _Faulty_FunctionTyperTest_language #-}
_Faulty_FunctionTyperTest_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/FunctionTyperTest.language" in seq str str
