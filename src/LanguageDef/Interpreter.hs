module LanguageDef.Interpreter where

{- Interprets functions -}

import Utils.All

import LanguageDef.LanguageDef
import LanguageDef.Syntax hiding (assert')
import LanguageDef.Function 
import LanguageDef.Expression 
import LanguageDef.Tools.LocationInfo
import LanguageDef.Tools.ExceptionInfo

import LanguageDef.LangDefs
import LanguageDef.Tools.Scope
import LanguageDef.Tools.Grouper

import qualified Data.Map as M
import Data.Map (Map)


{- | The type representing a variable store

>>> import LanguageDef.API
>>> import LanguageDef.Tools.Scope
>>> import LanguageDef.LangDefs


>>> let createPT tp p = createParseTree testLangage (testType tp) "?" p & crash' & removeHidden
>>> let createExp tp e = createTypedExpression testLangage "?" e (testType tp) & crash'
>>> let ld = testLangage' & get (ldScope . payload)

>>> let t te e tpt pt = patternMatch (const (), testLangage) M.empty ld (createExp te e) (createPT tpt pt)
>>> t "bool" "\"True\"" "bool" "True"
Success (fromList [])
>>> t "bool" "\"True\"" "bool" "False"
Left "Argument does not match expected token\nExpected: True\nGot: False\n"
>>> t "bool" "x" "bool" "True"
Success (fromList [("x",RuleEnter {_pt = Literal {_ptToken = "True", _ptLocation = ..., _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, _ptLocation = ..., _ptA = ()})])
>>> t "bool" "_" "bool" "True"
Success (fromList [])
>>> t "bool" "(x:bool)" "bool" "True"
Success (fromList [("x",RuleEnter {_pt = Literal {_ptToken = "True", _ptLocation = ...}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ...})])
>>> t "bool" "(x:bool)" "int" "5"
Left "Ascription failed\nExpected something of type TestLanguage.bool\nbut got a parsetree of the form TestLanguage.int\n"
>>> t "bool" "(x:bool)" "expr" "True"
Success (fromList [("x",RuleEnter {_pt = RuleEnter {_pt = Literal {_ptToken = "True", ..., _ptA = (), _ptHidden = False}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"expr"), _ptUsedIndex = 0, ..., _ptA = ()})])
>>> t "bool" "(x:bool)" "expr" "5"
Left "Ascription failed\nExpected something of type TestLanguage.bool\nbut got a parsetree of the form TestLanguage.int\n"
>>> t "bool" "x&(y:bool)" "bool" "True"
Success (fromList [("x",RuleEnter {_pt = Literal {_ptToken = "True", ..., _ptA = (), _ptHidden = False}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ..., _ptA = ()}),("y",RuleEnter {_pt = Literal {_ptToken = "True", ..., _ptA = (), _ptHidden = False}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ..., _ptA = ()})])
>>> t "exprSum" "x \"+\" y" "exprSum" "5 + 6 + 7"
Success (fromList [("x",RuleEnter {_pt = RuleEnter {_pt = Int {_ptInt = 5, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"int"), _ptUsedIndex = 0, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"expr"), _ptUsedIndex = 1, ..., _ptA = ()}),("y",RuleEnter {_pt = Seq {_pts = [RuleEnter {_pt = RuleEnter {_pt = Int {_ptInt = 6, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"int"), _ptUsedIndex = 0, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"expr"), _ptUsedIndex = 1, ..., _ptA = ()},RuleEnter {_pt = Literal {_ptToken = "+", ..., _ptA = (), _ptHidden = False}, _ptUsedRule = (["TestLanguage"],"op"), _ptUsedIndex = 1, ..., _ptA = ()},RuleEnter {_pt = RuleEnter {_pt = RuleEnter {_pt = Int {_ptInt = 7, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"int"), _ptUsedIndex = 0, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"expr"), _ptUsedIndex = 1, ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"exprSum"), _ptUsedIndex = 1, ..., _ptA = ()}], ..., _ptA = ()}, _ptUsedRule = (["TestLanguage"],"exprSum"), _ptUsedIndex = 0, ..., _ptA = ()})])





>>> tNot arg	= resolveAndRun (const ()) testLangage (["TestLanguage"], "not") [createPT "expr" arg] & either error toParsable
>>> tNot "True"
"False"
>>> tNot "False"
"True"

>>> let tNand a b	= resolveAndRun (const ()) testLangage (["TestLanguage"], "nand") [createPT "expr" a, createPT "expr" b] & either error toParsable
>>> tNand "False" "False"
"True"
>>> tNand "True" "False"
"True"
>>> tNand "False" "True"
"True"
>>> tNand "True" "True"
"False"

>>> let tOr a b	= resolveAndRun (const ()) testLangage (["TestLanguage"], "or") [createPT "expr" a, createPT "expr" b] & either error toParsable
>>> tOr "True" "True"
"True"
>>> tOr "True" "False"
"True"
>>> tOr "False" "True"
"True"
>>> tOr "False" "False"
"False"

-}

type VariableStore a
	= Map Name (ParseTree a)
 


-------------------------- ABOUT RUNNING A FUNCTION ------------------------------------


resolveAndRun'	:: LangDefs -> FQName -> [ParseTree ()] -> Failable (ParseTree ())
resolveAndRun'
	= resolveAndRun (const ())

{- | Resolves the function, executes it. The first arguments adds an annotation to the parsetree, based on the type of the parsetree

-}
resolveAndRun	:: (SyntFormIndex -> a) -> LangDefs -> FQName -> [ParseTree a] -> Failable (ParseTree a)
resolveAndRun fb2a lds (targetLD, name) args
	= do	ld	<- checkExistsSugg' dots targetLD (get langdefs lds) ("The module "++dots targetLD++" was not found")
		let ld'	= ld & get (ldScope . payload) 	:: LanguageDef
		let msg	= "The module "++dots targetLD++" does not have a function section and thus does not define "++name
		funcs	<- ld' & get langFunctions 
				& maybe (fail msg) return

		func	<- checkExistsSuggDist' (show, levenshtein) name (get grouperDict funcs) 
				$ "The module "++dots targetLD++" does not define the function "++show name
		runFunction fb2a lds ld' func args 

runFunction	::  (SyntFormIndex -> a) -> LangDefs -> LanguageDef -> Function -> [ParseTree a] -> Failable (ParseTree a)
runFunction fb2a lds ld func args
	= inMsg' ("While executing the function "++get funcName func) $ inLocation (get (funcDoc . miLoc) func) $ 
		do	let clauses	= func & get funcClauses
			clauses & mapi |> flip (runClause fb2a lds ld) args & firstSuccess			


runClause	:: (SyntFormIndex -> a) -> LangDefs -> LanguageDef -> (Int, FunctionClause SyntFormIndex) -> [ParseTree a] -> Failable (ParseTree a)
runClause fb2a lds ld (i, FunctionClause pats result doc nm) args
	= inMsg' ("While trying clause "++ nm ++ "." ++ show i) $ inLocation (get miLoc doc) $
	  do	store	<- patternMatchAll (fb2a, lds) M.empty ld pats args
		constructParseTree fb2a lds store result


------------------------- ABOUT PARSETREE CONSTRUCTION/INTERPRETATION --------------------------------


constructParseTree	:: (SyntFormIndex -> a) -> LangDefs -> VariableStore a -> Expression SyntFormIndex -> Failable (ParseTree a)
constructParseTree _ _ vars (Var nm _ _)
	= checkExistsSuggDist' (show, levenshtein) nm vars ("The variable "++show nm++" was not defined by the pattern")
constructParseTree _ _ _ DontCare{}
	= fail "Found a wildcard (_) in an expression context. Only use these as patterns!"
constructParseTree fb2a _ _ (ParseTree pt b _)
	= do	let a	= fb2a b
		pt |> const a & return
constructParseTree fb2a lds vars (FuncCall func args _ _)
	= do	args'	<- args |+> constructParseTree fb2a lds vars
		resolveAndRun fb2a lds func args'
constructParseTree fb2a lds vars (Ascription expr _ _ _)
	= constructParseTree fb2a lds vars expr -- the typechecker should ensure correctness, runtime check is unneeded here
constructParseTree fb2a lds vars (Split exp1 exp2 _ _)
	= do	pt1	<- constructParseTree fb2a lds vars exp1
		pt2	<- constructParseTree fb2a lds vars exp2
		let pt1'	= bareInformation pt1		
		let pt2'	= bareInformation pt2
		assert' (pt1' == pt2') $ unlines
			["When a split is used in an expression context, both generated results should be the same"
			, "First result: "++toParsable pt1'
			,"Generated by: "++toParsable exp1
			, "Second result: "++toParsable pt2'
			, "Generated by: "++toParsable exp2]
		return pt1
constructParseTree fb2a lds vars (SeqExp exprs b _)
	= do	pts	<- exprs |> constructParseTree fb2a lds vars & allGood
		return $ Seq pts unknownLocation (fb2a b)

------------------------ ABOUT PATTERN MATCHING ---------------------------------------


patternMatchAll	:: (SyntFormIndex -> a, LangDefs) -> VariableStore a -> LanguageDef -> [Expression SyntFormIndex] -> [ParseTree a]  -> Failable (VariableStore a)
patternMatchAll lds store ld [] []
	= return store
patternMatchAll lds store ld (expr:exprs) (arg:args)
	= inLocation (get expLocation expr) $ do
		assert' (length exprs == length args)
			$ "Number of arguments is incorrect: got "++show (length args)++" patterns, but got "++show (length exprs)++" arguments"
		store'	<- patternMatch lds store ld expr arg
		mergedStores	<- mergeStores [store, store']
		patternMatchAll lds mergedStores ld exprs args






patternMatch	:: (SyntFormIndex -> a, LangDefs) -> VariableStore a -> LanguageDef -> 
			Expression SyntFormIndex -> ParseTree a -> Failable (VariableStore a)
patternMatch _ _ _ (Var nm _ _) pt
	= return $ M.singleton nm pt
patternMatch _ _ _ (DontCare _ _) _
	= return M.empty
patternMatch _ _ _ (ParseTree expectedPT _ _) actPt
 	= do	let exp	= expectedPT & bareInformation
		let act	= actPt & bareInformation
		assert' (exp == act) $ unlines 
			["Argument does not match expected token"
			, "Expected: "++toParsable exp
			, "Got: "++toParsable act]
		return M.empty
patternMatch context@(fb2a, lds) vars ld (FuncCall funcName args b _) arg
	= inMsg' ("While running a function within a pattern, namely "++showFQ funcName) $
	  do	argsPt		<- args |+> constructParseTree fb2a lds vars
		expectedPT	<- resolveAndRun fb2a lds funcName argsPt |> deAnnot
		patternMatch context vars ld (ParseTree expectedPT b unknownLocation) arg
patternMatch lds vars ld (Ascription expr expType _ _) re@RuleEnter{}
	= do	let actType	= re & mostSpecificRuleEnter & get ptUsedRule
		assert' (isSubtypeOf ld actType expType) $ unlines
			["Ascription failed"
			, "Expected something of type "++showFQ expType
			, "but got a parsetree of the form "++showFQ actType]
		patternMatch lds vars ld expr re
patternMatch lds vars ld (Split exp1 exp2 _ _) pt
	= do	store1	<- patternMatch lds vars ld exp1 pt
		vars'	<- mergeStores [store1, vars]
		store2	<- patternMatch lds vars' ld exp2 pt
		mergeStores [store1, store2]
patternMatch lds vars ld (SeqExp exprs _ _) (Seq pts _ _)
	= patternMatchAll lds vars ld exprs pts
patternMatch lds vars ld expr (RuleEnter pt _ _ _ _)
	= patternMatch lds vars ld expr pt

patternMatch _ _ _ expr pt
	= fail $ "Could not match "++toParsable expr++" with "++toParsable pt


mergeStores	:: [VariableStore b] -> Failable (VariableStore b)
mergeStores stores
 	= do	let stores'	= stores ||>> return & M.unionsWith sameBareInfo
		stores' & M.toList 
			|> sndEffect & allGood
			|> M.fromList


sameBareInfo	:: Failable (ParseTree a) -> Failable (ParseTree a) -> Failable (ParseTree a)
sameBareInfo expectedPt actPt
	= do	exp	<- expectedPt |> bareInformation
		act	<- actPt |> bareInformation
		assert' (exp == act) $ unlines 
			["Variable store elements are not the same:"
			, "First instance: "++toParsable exp
			, "Other instance: "++toParsable act]
		actPt
