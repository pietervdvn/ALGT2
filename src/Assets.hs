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
allAssets	= [("Syntax.language", _Syntax_language)
			, ("Functions.language", _Functions_language)
			, ("TestLanguage.language", _TestLanguage_language)
			, ("RelationSyntax.language", _RelationSyntax_language)
			, ("TestInput/LoopingSupertypes.language", _TestInput_LoopingSupertypes_language)
			, ("TestInput/Nested/Z.language", _TestInput_Nested_Z_language)
			, ("TestInput/Nested/X.language", _TestInput_Nested_X_language)
			, ("TestInput/Nested/L.language", _TestInput_Nested_L_language)
			, ("TestInput/Nested/Y.language", _TestInput_Nested_Y_language)
			, ("Faulty/FunctionDuplicateNameTest.language", _Faulty_FunctionDuplicateNameTest_language)
			, ("Faulty/TestShadowing.language", _Faulty_TestShadowing_language)
			, ("Faulty/FunctionIncorrectNameTest.language", _Faulty_FunctionIncorrectNameTest_language)
			, ("Faulty/FunctionTyperTest.language", _Faulty_FunctionTyperTest_language)
			, ("Faulty/VariableTypingErrors.language", _Faulty_VariableTypingErrors_language)
			, ("MetaSyntax/Main.language", _MetaSyntax_Main_language)
			, ("MetaSyntax/Syntax.language", _MetaSyntax_Syntax_language)
			, ("MetaSyntax/Functions.language", _MetaSyntax_Functions_language)
			, ("MetaSyntax/Helper.language", _MetaSyntax_Helper_language)
			, ("TestLanguages/TestLanguageFaulty.language", _TestLanguages_TestLanguageFaulty_language)
			]

{-# NOINLINE _Syntax_language #-}
_Syntax_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Syntax.language" in seq str str

{-# NOINLINE _Functions_language #-}
_Functions_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Functions.language" in seq str str

{-# NOINLINE _TestLanguage_language #-}
_TestLanguage_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestLanguage.language" in seq str str

{-# NOINLINE _RelationSyntax_language #-}
_RelationSyntax_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/RelationSyntax.language" in seq str str

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

{-# NOINLINE _Faulty_FunctionDuplicateNameTest_language #-}
_Faulty_FunctionDuplicateNameTest_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/FunctionDuplicateNameTest.language" in seq str str

{-# NOINLINE _Faulty_TestShadowing_language #-}
_Faulty_TestShadowing_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/TestShadowing.language" in seq str str

{-# NOINLINE _Faulty_FunctionIncorrectNameTest_language #-}
_Faulty_FunctionIncorrectNameTest_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/FunctionIncorrectNameTest.language" in seq str str

{-# NOINLINE _Faulty_FunctionTyperTest_language #-}
_Faulty_FunctionTyperTest_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/FunctionTyperTest.language" in seq str str

{-# NOINLINE _Faulty_VariableTypingErrors_language #-}
_Faulty_VariableTypingErrors_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/VariableTypingErrors.language" in seq str str

{-# NOINLINE _MetaSyntax_Main_language #-}
_MetaSyntax_Main_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/MetaSyntax/Main.language" in seq str str

{-# NOINLINE _MetaSyntax_Syntax_language #-}
_MetaSyntax_Syntax_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/MetaSyntax/Syntax.language" in seq str str

{-# NOINLINE _MetaSyntax_Functions_language #-}
_MetaSyntax_Functions_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/MetaSyntax/Functions.language" in seq str str

{-# NOINLINE _MetaSyntax_Helper_language #-}
_MetaSyntax_Helper_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/MetaSyntax/Helper.language" in seq str str

{-# NOINLINE _TestLanguages_TestLanguageFaulty_language #-}
_TestLanguages_TestLanguageFaulty_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestLanguages/TestLanguageFaulty.language" in seq str str
