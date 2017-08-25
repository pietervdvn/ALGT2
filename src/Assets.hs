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
allAssets	= [("Main.language", _Main_language)
			, ("Syntax.language", _Syntax_language)
			, ("Functions.language", _Functions_language)
			, ("TestLanguage.language", _TestLanguage_language)
			, ("Helper.language", _Helper_language)
			, ("Relations.language", _Relations_language)
			, ("TestInput/LoopingSupertypes.language", _TestInput_LoopingSupertypes_language)
			, ("TestInput/Nested/Z.language", _TestInput_Nested_Z_language)
			, ("TestInput/Nested/X.language", _TestInput_Nested_X_language)
			, ("TestInput/Nested/L.language", _TestInput_Nested_L_language)
			, ("TestInput/Nested/Y.language", _TestInput_Nested_Y_language)
			, ("Faulty/TitleMismatch.language", _Faulty_TitleMismatch_language)
			, ("Faulty/FunctionDuplicateNameTest.language", _Faulty_FunctionDuplicateNameTest_language)
			, ("Faulty/TestShadowing.language", _Faulty_TestShadowing_language)
			, ("Faulty/FunctionIncorrectNameTest.language", _Faulty_FunctionIncorrectNameTest_language)
			, ("Faulty/FunctionTyperTest.language", _Faulty_FunctionTyperTest_language)
			, ("Faulty/VariableTypingErrors.language", _Faulty_VariableTypingErrors_language)
			, ("Faulty/Relations/NotLocal.language", _Faulty_Relations_NotLocal_language)
			, ("Faulty/Relations/DuplicateRelation.language", _Faulty_Relations_DuplicateRelation_language)
			, ("Faulty/Relations/EmptyLine.language", _Faulty_Relations_EmptyLine_language)
			, ("Faulty/Relations/UnknownTypeRelation.language", _Faulty_Relations_UnknownTypeRelation_language)
			, ("Faulty/Relations/AllOutRel.language", _Faulty_Relations_AllOutRel_language)
			, ("Faulty/Relations/NotDeclared.language", _Faulty_Relations_NotDeclared_language)
			, ("Faulty/Relations/UnkownTypeRelation.language", _Faulty_Relations_UnkownTypeRelation_language)
			, ("MetaSyntax/Main.language", _MetaSyntax_Main_language)
			, ("MetaSyntax/Syntax.language", _MetaSyntax_Syntax_language)
			, ("MetaSyntax/Functions.language", _MetaSyntax_Functions_language)
			, ("MetaSyntax/Helper.language", _MetaSyntax_Helper_language)
			, ("MetaSyntax/Relations.language", _MetaSyntax_Relations_language)
			, ("TestLanguages/TestLanguageFaulty.language", _TestLanguages_TestLanguageFaulty_language)
			]

{-# NOINLINE _Main_language #-}
_Main_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Main.language" in seq str str

{-# NOINLINE _Syntax_language #-}
_Syntax_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Syntax.language" in seq str str

{-# NOINLINE _Functions_language #-}
_Functions_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Functions.language" in seq str str

{-# NOINLINE _TestLanguage_language #-}
_TestLanguage_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestLanguage.language" in seq str str

{-# NOINLINE _Helper_language #-}
_Helper_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Helper.language" in seq str str

{-# NOINLINE _Relations_language #-}
_Relations_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Relations.language" in seq str str

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

{-# NOINLINE _Faulty_TitleMismatch_language #-}
_Faulty_TitleMismatch_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/TitleMismatch.language" in seq str str

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

{-# NOINLINE _Faulty_Relations_NotLocal_language #-}
_Faulty_Relations_NotLocal_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/NotLocal.language" in seq str str

{-# NOINLINE _Faulty_Relations_DuplicateRelation_language #-}
_Faulty_Relations_DuplicateRelation_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/DuplicateRelation.language" in seq str str

{-# NOINLINE _Faulty_Relations_EmptyLine_language #-}
_Faulty_Relations_EmptyLine_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/EmptyLine.language" in seq str str

{-# NOINLINE _Faulty_Relations_UnknownTypeRelation_language #-}
_Faulty_Relations_UnknownTypeRelation_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/UnknownTypeRelation.language" in seq str str

{-# NOINLINE _Faulty_Relations_AllOutRel_language #-}
_Faulty_Relations_AllOutRel_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/AllOutRel.language" in seq str str

{-# NOINLINE _Faulty_Relations_NotDeclared_language #-}
_Faulty_Relations_NotDeclared_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/NotDeclared.language" in seq str str

{-# NOINLINE _Faulty_Relations_UnkownTypeRelation_language #-}
_Faulty_Relations_UnkownTypeRelation_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/UnkownTypeRelation.language" in seq str str

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

{-# NOINLINE _MetaSyntax_Relations_language #-}
_MetaSyntax_Relations_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/MetaSyntax/Relations.language" in seq str str

{-# NOINLINE _TestLanguages_TestLanguageFaulty_language #-}
_TestLanguages_TestLanguageFaulty_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestLanguages/TestLanguageFaulty.language" in seq str str
