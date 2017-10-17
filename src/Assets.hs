module Assets where


import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (toStrict)
import System.IO
import Control.DeepSeq
import Data.Time

-- Automatically generated
-- This file contains all assets, loaded via 'unsafePerformIO' or hardcoded as string, not to need IO for assets

readFile'		:: FilePath -> IO String
readFile' str = do	h <- openFile str ReadMode
			s <- hGetContents h
			s `deepseq` hClose h
			return s

timeCreated	:: UTCTime
timeCreated	= read "2017-10-17 00:20:34.995536 UTC"
allAssets	:: [(FilePath, String)]
allAssets	= [("Functions.language", _Functions_language)
			, ("TestLanguage.language", _TestLanguage_language)
			, ("Resources/Template.language", _Resources_Template_language)
			, ("TestInput/LoopingSupertypes.language", _TestInput_LoopingSupertypes_language)
			, ("TestInput/Nested/Z.language", _TestInput_Nested_Z_language)
			, ("TestInput/Nested/X.language", _TestInput_Nested_X_language)
			, ("TestInput/Nested/L.language", _TestInput_Nested_L_language)
			, ("TestInput/Nested/Y.language", _TestInput_Nested_Y_language)
			, ("Faulty/TitleMismatch.language", _Faulty_TitleMismatch_language)
			, ("Faulty/LeftRecursiveSyntax.language", _Faulty_LeftRecursiveSyntax_language)
			, ("Faulty/FunctionDuplicateNameTest.language", _Faulty_FunctionDuplicateNameTest_language)
			, ("Faulty/TestShadowing.language", _Faulty_TestShadowing_language)
			, ("Faulty/SyntaxUndeclared.language", _Faulty_SyntaxUndeclared_language)
			, ("Faulty/FunctionIncorrectNameTest.language", _Faulty_FunctionIncorrectNameTest_language)
			, ("Faulty/FunctionTyperTest.language", _Faulty_FunctionTyperTest_language)
			, ("Faulty/VariableTypingErrors.language", _Faulty_VariableTypingErrors_language)
			, ("Faulty/Relations/NotLocal.language", _Faulty_Relations_NotLocal_language)
			, ("Faulty/Relations/DuplicateRelation.language", _Faulty_Relations_DuplicateRelation_language)
			, ("Faulty/Relations/EmptyLine.language", _Faulty_Relations_EmptyLine_language)
			, ("Faulty/Relations/UnknownTypeRelation.language", _Faulty_Relations_UnknownTypeRelation_language)
			, ("Faulty/Relations/IncorrectRule.language", _Faulty_Relations_IncorrectRule_language)
			, ("Faulty/Relations/AllOutRel.language", _Faulty_Relations_AllOutRel_language)
			, ("Faulty/Relations/NotDeclared.language", _Faulty_Relations_NotDeclared_language)
			, ("Faulty/Relations/Relation.language", _Faulty_Relations_Relation_language)
			, ("Faulty/Relations/TypeErr.language", _Faulty_Relations_TypeErr_language)
			, ("Faulty/Relations/UnkownTypeRelation.language", _Faulty_Relations_UnkownTypeRelation_language)
			, ("ALGT/Builtins.language", _ALGT_Builtins_language)
			, ("ALGT/Readme.md", _ALGT_Readme_md)
			, ("ALGT/Native/Syntax.language", _ALGT_Native_Syntax_language)
			, ("ALGT/Native/Functions.language", _ALGT_Native_Functions_language)
			, ("ALGT/Native/ALGT.language", _ALGT_Native_ALGT_language)
			, ("ALGT/Native/Helper.language", _ALGT_Native_Helper_language)
			, ("ALGT/Native/Relations.language", _ALGT_Native_Relations_language)
			, ("ALGT/Sugared/Syntax.language", _ALGT_Sugared_Syntax_language)
			, ("MetaSyntax/Syntax.language", _MetaSyntax_Syntax_language)
			, ("MetaSyntax/Functions.language", _MetaSyntax_Functions_language)
			, ("MetaSyntax/ALGT.language", _MetaSyntax_ALGT_language)
			, ("MetaSyntax/Helper.language", _MetaSyntax_Helper_language)
			, ("MetaSyntax/Relations.language", _MetaSyntax_Relations_language)
			, ("TestLanguages/STFL.language", _TestLanguages_STFL_language)
			]

{-# NOINLINE _Functions_language #-}
_Functions_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Functions.language" in seq str str

{-# NOINLINE _TestLanguage_language #-}
_TestLanguage_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestLanguage.language" in seq str str

{-# NOINLINE _Resources_Template_language #-}
_Resources_Template_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Resources/Template.language" in seq str str

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

{-# NOINLINE _Faulty_LeftRecursiveSyntax_language #-}
_Faulty_LeftRecursiveSyntax_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/LeftRecursiveSyntax.language" in seq str str

{-# NOINLINE _Faulty_FunctionDuplicateNameTest_language #-}
_Faulty_FunctionDuplicateNameTest_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/FunctionDuplicateNameTest.language" in seq str str

{-# NOINLINE _Faulty_TestShadowing_language #-}
_Faulty_TestShadowing_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/TestShadowing.language" in seq str str

{-# NOINLINE _Faulty_SyntaxUndeclared_language #-}
_Faulty_SyntaxUndeclared_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/SyntaxUndeclared.language" in seq str str

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

{-# NOINLINE _Faulty_Relations_IncorrectRule_language #-}
_Faulty_Relations_IncorrectRule_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/IncorrectRule.language" in seq str str

{-# NOINLINE _Faulty_Relations_AllOutRel_language #-}
_Faulty_Relations_AllOutRel_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/AllOutRel.language" in seq str str

{-# NOINLINE _Faulty_Relations_NotDeclared_language #-}
_Faulty_Relations_NotDeclared_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/NotDeclared.language" in seq str str

{-# NOINLINE _Faulty_Relations_Relation_language #-}
_Faulty_Relations_Relation_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/Relation.language" in seq str str

{-# NOINLINE _Faulty_Relations_TypeErr_language #-}
_Faulty_Relations_TypeErr_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/TypeErr.language" in seq str str

{-# NOINLINE _Faulty_Relations_UnkownTypeRelation_language #-}
_Faulty_Relations_UnkownTypeRelation_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/Faulty/Relations/UnkownTypeRelation.language" in seq str str

{-# NOINLINE _ALGT_Builtins_language #-}
_ALGT_Builtins_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/ALGT/Builtins.language" in seq str str

{-# NOINLINE _ALGT_Readme_md #-}
_ALGT_Readme_md
	 = let str = unsafePerformIO $ readFile' "src/Assets/ALGT/Readme.md" in seq str str

{-# NOINLINE _ALGT_Native_Syntax_language #-}
_ALGT_Native_Syntax_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/ALGT/Native/Syntax.language" in seq str str

{-# NOINLINE _ALGT_Native_Functions_language #-}
_ALGT_Native_Functions_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/ALGT/Native/Functions.language" in seq str str

{-# NOINLINE _ALGT_Native_ALGT_language #-}
_ALGT_Native_ALGT_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/ALGT/Native/ALGT.language" in seq str str

{-# NOINLINE _ALGT_Native_Helper_language #-}
_ALGT_Native_Helper_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/ALGT/Native/Helper.language" in seq str str

{-# NOINLINE _ALGT_Native_Relations_language #-}
_ALGT_Native_Relations_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/ALGT/Native/Relations.language" in seq str str

{-# NOINLINE _ALGT_Sugared_Syntax_language #-}
_ALGT_Sugared_Syntax_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/ALGT/Sugared/Syntax.language" in seq str str

{-# NOINLINE _MetaSyntax_Syntax_language #-}
_MetaSyntax_Syntax_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/MetaSyntax/Syntax.language" in seq str str

{-# NOINLINE _MetaSyntax_Functions_language #-}
_MetaSyntax_Functions_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/MetaSyntax/Functions.language" in seq str str

{-# NOINLINE _MetaSyntax_ALGT_language #-}
_MetaSyntax_ALGT_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/MetaSyntax/ALGT.language" in seq str str

{-# NOINLINE _MetaSyntax_Helper_language #-}
_MetaSyntax_Helper_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/MetaSyntax/Helper.language" in seq str str

{-# NOINLINE _MetaSyntax_Relations_language #-}
_MetaSyntax_Relations_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/MetaSyntax/Relations.language" in seq str str

{-# NOINLINE _TestLanguages_STFL_language #-}
_TestLanguages_STFL_language
	 = let str = unsafePerformIO $ readFile' "src/Assets/TestLanguages/STFL.language" in seq str str
