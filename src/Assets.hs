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
timeCreated	= read "2019-12-20 01:25:32.058331298 UTC"
allAssets	:: [(FilePath, String)]
allAssets	= [("Functions.language", _Functions_language)
			, ("Readme.md", _Readme_md)
			, ("TestLanguage.language", _TestLanguage_language)
			, ("MetaSyntax/ALGT.language", _MetaSyntax_ALGT_language)
			, ("MetaSyntax/Functions.language", _MetaSyntax_Functions_language)
			, ("MetaSyntax/Syntax.language", _MetaSyntax_Syntax_language)
			, ("MetaSyntax/Relations.language", _MetaSyntax_Relations_language)
			, ("MetaSyntax/Helper.language", _MetaSyntax_Helper_language)
			, ("ALGT/Builtins.language", _ALGT_Builtins_language)
			, ("ALGT/Readme.md", _ALGT_Readme_md)
			, ("ALGT/Sugared/Syntax.language", _ALGT_Sugared_Syntax_language)
			, ("ALGT/Native/ALGT.language", _ALGT_Native_ALGT_language)
			, ("ALGT/Native/Functions.language", _ALGT_Native_Functions_language)
			, ("ALGT/Native/Syntax.language", _ALGT_Native_Syntax_language)
			, ("ALGT/Native/Relations.language", _ALGT_Native_Relations_language)
			, ("ALGT/Native/Helper.language", _ALGT_Native_Helper_language)
			, ("TestLanguages/STFL.language", _TestLanguages_STFL_language)
			, ("TestInput/LoopingSupertypes.language", _TestInput_LoopingSupertypes_language)
			, ("TestInput/Faulty/TestShadowing.language", _TestInput_Faulty_TestShadowing_language)
			, ("TestInput/Faulty/FunctionDuplicateNameTest.language", _TestInput_Faulty_FunctionDuplicateNameTest_language)
			, ("TestInput/Faulty/TitleMismatch.language", _TestInput_Faulty_TitleMismatch_language)
			, ("TestInput/Faulty/SyntaxUndeclared.language", _TestInput_Faulty_SyntaxUndeclared_language)
			, ("TestInput/Faulty/FunctionIncorrectNameTest.language", _TestInput_Faulty_FunctionIncorrectNameTest_language)
			, ("TestInput/Faulty/LeftRecursiveSyntax.language", _TestInput_Faulty_LeftRecursiveSyntax_language)
			, ("TestInput/Faulty/FunctionTyperTest.language", _TestInput_Faulty_FunctionTyperTest_language)
			, ("TestInput/Faulty/VariableTypingErrors.language", _TestInput_Faulty_VariableTypingErrors_language)
			, ("TestInput/Faulty/Relations/Relation.language", _TestInput_Faulty_Relations_Relation_language)
			, ("TestInput/Faulty/Relations/TypeErr.language", _TestInput_Faulty_Relations_TypeErr_language)
			, ("TestInput/Faulty/Relations/NotDeclared.language", _TestInput_Faulty_Relations_NotDeclared_language)
			, ("TestInput/Faulty/Relations/UnknownTypeRelation.language", _TestInput_Faulty_Relations_UnknownTypeRelation_language)
			, ("TestInput/Faulty/Relations/IncorrectRule.language", _TestInput_Faulty_Relations_IncorrectRule_language)
			, ("TestInput/Faulty/Relations/AllOutRel.language", _TestInput_Faulty_Relations_AllOutRel_language)
			, ("TestInput/Faulty/Relations/UnkownTypeRelation.language", _TestInput_Faulty_Relations_UnkownTypeRelation_language)
			, ("TestInput/Faulty/Relations/EmptyLine.language", _TestInput_Faulty_Relations_EmptyLine_language)
			, ("TestInput/Faulty/Relations/DuplicateRelation.language", _TestInput_Faulty_Relations_DuplicateRelation_language)
			, ("TestInput/Faulty/Relations/NotLocal.language", _TestInput_Faulty_Relations_NotLocal_language)
			, ("TestInput/Nested/L.language", _TestInput_Nested_L_language)
			, ("TestInput/Nested/X.language", _TestInput_Nested_X_language)
			, ("TestInput/Nested/Y.language", _TestInput_Nested_Y_language)
			, ("TestInput/Nested/Z.language", _TestInput_Nested_Z_language)
			, ("Resources/Template.language", _Resources_Template_language)
			, ("Resources/InterpreterHelp.md", _Resources_InterpreterHelp_md)
			, ("Resources/Readme.md", _Resources_Readme_md)
			, ("Resources/RuleSimple.svg", _Resources_RuleSimple_svg)
			]

{-# NOINLINE _Functions_language #-}
_Functions_language
	 = let str = unsafePerformIO $ readFile' "Assets/Functions.language" in seq str str

{-# NOINLINE _Readme_md #-}
_Readme_md
	 = let str = unsafePerformIO $ readFile' "Assets/Readme.md" in seq str str

{-# NOINLINE _TestLanguage_language #-}
_TestLanguage_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestLanguage.language" in seq str str

{-# NOINLINE _MetaSyntax_ALGT_language #-}
_MetaSyntax_ALGT_language
	 = let str = unsafePerformIO $ readFile' "Assets/MetaSyntax/ALGT.language" in seq str str

{-# NOINLINE _MetaSyntax_Functions_language #-}
_MetaSyntax_Functions_language
	 = let str = unsafePerformIO $ readFile' "Assets/MetaSyntax/Functions.language" in seq str str

{-# NOINLINE _MetaSyntax_Syntax_language #-}
_MetaSyntax_Syntax_language
	 = let str = unsafePerformIO $ readFile' "Assets/MetaSyntax/Syntax.language" in seq str str

{-# NOINLINE _MetaSyntax_Relations_language #-}
_MetaSyntax_Relations_language
	 = let str = unsafePerformIO $ readFile' "Assets/MetaSyntax/Relations.language" in seq str str

{-# NOINLINE _MetaSyntax_Helper_language #-}
_MetaSyntax_Helper_language
	 = let str = unsafePerformIO $ readFile' "Assets/MetaSyntax/Helper.language" in seq str str

{-# NOINLINE _ALGT_Builtins_language #-}
_ALGT_Builtins_language
	 = let str = unsafePerformIO $ readFile' "Assets/ALGT/Builtins.language" in seq str str

{-# NOINLINE _ALGT_Readme_md #-}
_ALGT_Readme_md
	 = let str = unsafePerformIO $ readFile' "Assets/ALGT/Readme.md" in seq str str

{-# NOINLINE _ALGT_Sugared_Syntax_language #-}
_ALGT_Sugared_Syntax_language
	 = let str = unsafePerformIO $ readFile' "Assets/ALGT/Sugared/Syntax.language" in seq str str

{-# NOINLINE _ALGT_Native_ALGT_language #-}
_ALGT_Native_ALGT_language
	 = let str = unsafePerformIO $ readFile' "Assets/ALGT/Native/ALGT.language" in seq str str

{-# NOINLINE _ALGT_Native_Functions_language #-}
_ALGT_Native_Functions_language
	 = let str = unsafePerformIO $ readFile' "Assets/ALGT/Native/Functions.language" in seq str str

{-# NOINLINE _ALGT_Native_Syntax_language #-}
_ALGT_Native_Syntax_language
	 = let str = unsafePerformIO $ readFile' "Assets/ALGT/Native/Syntax.language" in seq str str

{-# NOINLINE _ALGT_Native_Relations_language #-}
_ALGT_Native_Relations_language
	 = let str = unsafePerformIO $ readFile' "Assets/ALGT/Native/Relations.language" in seq str str

{-# NOINLINE _ALGT_Native_Helper_language #-}
_ALGT_Native_Helper_language
	 = let str = unsafePerformIO $ readFile' "Assets/ALGT/Native/Helper.language" in seq str str

{-# NOINLINE _TestLanguages_STFL_language #-}
_TestLanguages_STFL_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestLanguages/STFL.language" in seq str str

{-# NOINLINE _TestInput_LoopingSupertypes_language #-}
_TestInput_LoopingSupertypes_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/LoopingSupertypes.language" in seq str str

{-# NOINLINE _TestInput_Faulty_TestShadowing_language #-}
_TestInput_Faulty_TestShadowing_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/TestShadowing.language" in seq str str

{-# NOINLINE _TestInput_Faulty_FunctionDuplicateNameTest_language #-}
_TestInput_Faulty_FunctionDuplicateNameTest_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/FunctionDuplicateNameTest.language" in seq str str

{-# NOINLINE _TestInput_Faulty_TitleMismatch_language #-}
_TestInput_Faulty_TitleMismatch_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/TitleMismatch.language" in seq str str

{-# NOINLINE _TestInput_Faulty_SyntaxUndeclared_language #-}
_TestInput_Faulty_SyntaxUndeclared_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/SyntaxUndeclared.language" in seq str str

{-# NOINLINE _TestInput_Faulty_FunctionIncorrectNameTest_language #-}
_TestInput_Faulty_FunctionIncorrectNameTest_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/FunctionIncorrectNameTest.language" in seq str str

{-# NOINLINE _TestInput_Faulty_LeftRecursiveSyntax_language #-}
_TestInput_Faulty_LeftRecursiveSyntax_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/LeftRecursiveSyntax.language" in seq str str

{-# NOINLINE _TestInput_Faulty_FunctionTyperTest_language #-}
_TestInput_Faulty_FunctionTyperTest_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/FunctionTyperTest.language" in seq str str

{-# NOINLINE _TestInput_Faulty_VariableTypingErrors_language #-}
_TestInput_Faulty_VariableTypingErrors_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/VariableTypingErrors.language" in seq str str

{-# NOINLINE _TestInput_Faulty_Relations_Relation_language #-}
_TestInput_Faulty_Relations_Relation_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/Relations/Relation.language" in seq str str

{-# NOINLINE _TestInput_Faulty_Relations_TypeErr_language #-}
_TestInput_Faulty_Relations_TypeErr_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/Relations/TypeErr.language" in seq str str

{-# NOINLINE _TestInput_Faulty_Relations_NotDeclared_language #-}
_TestInput_Faulty_Relations_NotDeclared_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/Relations/NotDeclared.language" in seq str str

{-# NOINLINE _TestInput_Faulty_Relations_UnknownTypeRelation_language #-}
_TestInput_Faulty_Relations_UnknownTypeRelation_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/Relations/UnknownTypeRelation.language" in seq str str

{-# NOINLINE _TestInput_Faulty_Relations_IncorrectRule_language #-}
_TestInput_Faulty_Relations_IncorrectRule_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/Relations/IncorrectRule.language" in seq str str

{-# NOINLINE _TestInput_Faulty_Relations_AllOutRel_language #-}
_TestInput_Faulty_Relations_AllOutRel_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/Relations/AllOutRel.language" in seq str str

{-# NOINLINE _TestInput_Faulty_Relations_UnkownTypeRelation_language #-}
_TestInput_Faulty_Relations_UnkownTypeRelation_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/Relations/UnkownTypeRelation.language" in seq str str

{-# NOINLINE _TestInput_Faulty_Relations_EmptyLine_language #-}
_TestInput_Faulty_Relations_EmptyLine_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/Relations/EmptyLine.language" in seq str str

{-# NOINLINE _TestInput_Faulty_Relations_DuplicateRelation_language #-}
_TestInput_Faulty_Relations_DuplicateRelation_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/Relations/DuplicateRelation.language" in seq str str

{-# NOINLINE _TestInput_Faulty_Relations_NotLocal_language #-}
_TestInput_Faulty_Relations_NotLocal_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Faulty/Relations/NotLocal.language" in seq str str

{-# NOINLINE _TestInput_Nested_L_language #-}
_TestInput_Nested_L_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Nested/L.language" in seq str str

{-# NOINLINE _TestInput_Nested_X_language #-}
_TestInput_Nested_X_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Nested/X.language" in seq str str

{-# NOINLINE _TestInput_Nested_Y_language #-}
_TestInput_Nested_Y_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Nested/Y.language" in seq str str

{-# NOINLINE _TestInput_Nested_Z_language #-}
_TestInput_Nested_Z_language
	 = let str = unsafePerformIO $ readFile' "Assets/TestInput/Nested/Z.language" in seq str str

{-# NOINLINE _Resources_Template_language #-}
_Resources_Template_language
	 = let str = unsafePerformIO $ readFile' "Assets/Resources/Template.language" in seq str str

{-# NOINLINE _Resources_InterpreterHelp_md #-}
_Resources_InterpreterHelp_md
	 = let str = unsafePerformIO $ readFile' "Assets/Resources/InterpreterHelp.md" in seq str str

{-# NOINLINE _Resources_Readme_md #-}
_Resources_Readme_md
	 = let str = unsafePerformIO $ readFile' "Assets/Resources/Readme.md" in seq str str

{-# NOINLINE _Resources_RuleSimple_svg #-}
_Resources_RuleSimple_svg
	 = let str = unsafePerformIO $ readFile' "Assets/Resources/RuleSimple.svg" in seq str str
