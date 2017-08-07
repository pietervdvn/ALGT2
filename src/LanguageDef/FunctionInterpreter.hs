module LanguageDef.FunctionInterpreter where

{- Interprets functions -}

import Utils.All

import LanguageDef.LanguageDef
import LanguageDef.Syntax
import LanguageDef.MetaFunction 
import LanguageDef.MetaExpression 
import LanguageDef.LocationInfo

import qualified Data.Map as M
import Data.Map (Map)

{- | The type representing a variable store

>>> import LanguageDef.API
>>> import AssetUtils
>>> import LanguageDef.Scope
>>> import LanguageDef.LangDefs


>>> let createPT tp p = createParseTree testLangDefs (testType tp) "?" p & either error id	& removeHidden
>>> let createExp tp e = createTypedExpression testLangDefs "?" e (testType tp) & either error id
>>> let ld = AssetUtils.testLDScope & get (ldScope . payload)

>>> let t te e tpt pt = patternMatch ld (createExp te e) (createPT tpt pt)
>>> t "bool" "\"True\"" "bool" "True"
Right (fromList [])
>>> t "bool" "\"True\"" "bool" "False"
Left "Argument does not match expected token\nExpected: True\nGot: False\n"
>>> t "bool" "x" "bool" "True"
Right (fromList [("x",RuleEnter {_pt = Literal {_ptToken = "True", _ptLocation = ..., _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, _ptLocation = ..., _ptA = ()})])
>>> t "bool" "_" "bool" "True"
Right (fromList [])
>>> t "bool" "(x:bool)" "bool" "True"
Right (fromList [("x",RuleEnter {_pt = Literal {_ptToken = "True", _ptLocation = ...}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ...})])
>>> t "bool" "(x:bool)" "int" "5"
Left "Ascription failed\nExpected something of type TestLanguage.bool\nbut got a parsetree of the form TestLanguage.int\n"
>>> t "bool" "(x:bool)" "expr" "True"
Right (fromList [("x",RuleEnter {_pt = RuleEnter {_pt = Literal {_ptToken = "True", ..., _ptA = (), _ptHidden = False}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"expr"), _ptUsedIndex = 0, ..., _ptA = ()})])
>>> t "bool" "(x:bool)" "expr" "5"
Left "Ascription failed\nExpected something of type TestLanguage.bool\nbut got a parsetree of the form TestLanguage.int\n"
>>> t "bool" "x&(y:bool)" "bool" "True"
Right (fromList [("x",RuleEnter {_pt = Literal {_ptToken = "True", ..., _ptA = (), _ptHidden = False}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ..., _ptA = ()}),("y",RuleEnter {_pt = Literal {_ptToken = "True", ..., _ptA = (), _ptHidden = False}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ..., _ptA = ()})])
>>> t "exprSum" "x \"+\" y" "exprSum" "5 + 6 + 7"
Right (fromList [("x",RuleEnter {_pt = RuleEnter {_pt = Int {_ptInt = 5, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"int"), _ptUsedIndex = 0, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"expr"), _ptUsedIndex = 1, ..., _ptA = ()}),("y",RuleEnter {_pt = Seq {_pts = [RuleEnter {_pt = RuleEnter {_pt = Int {_ptInt = 6, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"int"), _ptUsedIndex = 0, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"expr"), _ptUsedIndex = 1, ..., _ptA = ()},RuleEnter {_pt = Literal {_ptToken = "+", ..., _ptA = (), _ptHidden = False}, _ptUsedRule = (["TestLanguage"],"op"), _ptUsedIndex = 1, ..., _ptA = ()},RuleEnter {_pt = RuleEnter {_pt = RuleEnter {_pt = Int {_ptInt = 7, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"int"), _ptUsedIndex = 0, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"expr"), _ptUsedIndex = 1, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"exprSum"), _ptUsedIndex = 1, ..., _ptA = ()}], ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"exprSum"), _ptUsedIndex = 0, ..., _ptA = ()})])
-}

type VariableStore a
	= Map Name (ParseTree a)


patternMatchAll	:: LanguageDef' ResolvedImport fr -> [Expression a] -> [ParseTree b]  -> Either String (VariableStore b)
patternMatchAll ld exprs pts
	= do	assert (length exprs == length pts) $ "Number of arguments is incorrect: got "++show (length pts)++" patterns, but got "++show (length exprs)++" arguments"
		stores	<- zip exprs pts |> uncurry (patternMatch ld) & allRight'
		mergeStores stores






patternMatch	:: LanguageDef' ResolvedImport fr -> 
			Expression a -> ParseTree b -> Either String (VariableStore b)
patternMatch _ (Var nm _) pt
	= return $ M.singleton nm pt
patternMatch _ (DontCare _) _
	= return M.empty
patternMatch _ (ParseTree expectedPT _) actPt
 	= do	let exp	= expectedPT & bareInformation
		let act	= actPt & bareInformation
		assert (exp == act) $ unlines 
			["Argument does not match expected token"
			, "Expected: "++toParsable exp
			, "Got: "++toParsable act]
		return M.empty
-- TODO | FuncCall {_expCallName :: LanguageDef.LocationInfo.FQName,
--             _expCallcArgs :: [Expression a],
--              _expAnnot :: a}
patternMatch ld (Ascription expr expType _) re@RuleEnter{}
	= do	let actType	= re & mostSpecificRuleEnter & get ptUsedRule
		assert (isSubtypeOf ld actType expType) $ unlines
			["Ascription failed"
			, "Expected something of type "++showFQ expType
			, "but got a parsetree of the form "++showFQ actType]
		patternMatch ld expr re
patternMatch ld (Split exp1 exp2 _) pt
	= do	store1	<- patternMatch ld exp1 pt
		store2	<- patternMatch ld exp2 pt
		mergeStores [store1, store2]
patternMatch ld (SeqExp exprs _) (Seq pts _ _)
	= patternMatchAll ld exprs pts
patternMatch ld expr (RuleEnter pt _ _ _ _)
	= patternMatch ld expr pt

patternMatch _ expr pt
	= Left $ "TODO: "++toParsable expr++" ~ \n "++asTree pt


mergeStores	:: [VariableStore b] -> Either String (VariableStore b)
mergeStores stores
 	= do	let stores'	= stores ||>> Right & M.unionsWith sameBareInfo
		stores' & M.toList 
			|> sndEffect & allRight'
			|> M.fromList


sameBareInfo	:: Either String (ParseTree a) -> Either String (ParseTree a) -> Either String (ParseTree a)
sameBareInfo expectedPt actPt
	= do	exp	<- expectedPt |> bareInformation
		act	<- actPt |> bareInformation
		assert (exp == act) $ unlines 
			["Variable store elements are not the same:"
			, "First instance: "++toParsable exp
			, "Other instance: "++toParsable act]
		actPt
