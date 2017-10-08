module LanguageDef.Interpreter (resolveAndRun, constructParseTree, VariableStore', VariableStore, patternMatchAll, evalExpression) where

{- Interprets functions -}

import Utils.All

import LanguageDef.Data.LanguageDef
import LanguageDef.Data.SyntacticForm hiding (assert')
import LanguageDef.Data.ParseTree
import LanguageDef.Data.SyntFormIndex
import LanguageDef.Data.Function 
import LanguageDef.Data.Expression 
import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.Grouper

import LanguageDef.LangDefs
import qualified LanguageDef.Builtins as Builtins

import qualified Data.Map as M
import Data.Map (Map)


{- | The type representing a variable store

>>> import LanguageDef.API
>>> import LanguageDef.LangDefs

>>> let testType x	= (["TestLanguage"], x)
>>> let createPT tp p = createParseTree testLanguage (testType tp) "?" p & crash' & removeHidden
>>> let createExp tp e = createTypedExpression testLanguage "?" e (testType tp) & crash'

>>> let t te e tpt pt = patternMatch testLanguage M.empty (createExp te e) (createPT tpt pt)
>>> t "bool" "\"True\"" "bool" "True"
Success (fromList [])
>>> t "bool" "\"True\"" "bool" "False"
Failed (ExceptionInfo {_errMsg = "Argument does not match expected token\nExpected: True\nGot: False\n", _errSeverity = Error, _errSuggestion = Nothing})
>>> t "bool" "x" "bool" "True"
Success (fromList [("x",RuleEnter {_pt = Literal {_ptToken = "True", _ptLocation = ..., _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, _ptLocation = ...})])
>>> t "bool" "_" "bool" "True"
Success (fromList [])
>>> t "bool" "(x:bool)" "bool" "True"
Success (fromList [("x",RuleEnter {_pt = Literal {_ptToken = "True", _ptLocation = ...}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ...})])
>>> t "bool" "(x:bool)" "int" "5"
Failed (ExceptionInfo {_errMsg = "Ascription failed\nExpected something of type TestLanguage.bool\nbut got a parsetree of the form TestLanguage.int\n", _errSeverity = Error, _errSuggestion = Nothing})
>>> t "bool" "(x:bool)" "expr" "True"
Success (fromList [("x",RuleEnter {_pt = RuleEnter {_pt = Literal {_ptToken = "True", ..., _ptHidden = False}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ...}, _ptUsedRule = (["TestLanguage"],"expr"), _ptUsedIndex = 0, ...})])
>>> t "bool" "(x:bool)" "expr" "5"
Failed (ExceptionInfo {_errMsg = "Ascription failed\nExpected something of type TestLanguage.bool\nbut got a parsetree of the form TestLanguage.int\n", _errSeverity = Error, _errSuggestion = Nothing})
>>> t "bool" "x&(y:bool)" "bool" "True"
Success (fromList [("x",RuleEnter {_pt = Literal {_ptToken = "True", ..., _ptHidden = False}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ...}),("y",RuleEnter {_pt = Literal {_ptToken = "True", ..., _ptHidden = False}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ...})])
>>> t "exprSum" "x \"+\" y" "exprSum" "5 + 6 + 7"
Success (fromList [("x",RuleEnter {_pt = RuleEnter {_pt = Int {_ptInt = 5, ...}, _ptUsedRule = (["TestLanguage"],"int"), _ptUsedIndex = 0, ...}, _ptUsedRule = (["TestLanguage"],"expr"), _ptUsedIndex = 1, ...}),("y",RuleEnter {_pt = Seq {_pts = [RuleEnter {_pt = RuleEnter {_pt = Int {_ptInt = 6, ...}, _ptUsedRule = (["TestLanguage"],"int"), _ptUsedIndex = 0, ...}, _ptUsedRule = (["TestLanguage"],"expr"), _ptUsedIndex = 1, ...},RuleEnter {_pt = Literal {_ptToken = "+", ..., _ptHidden = False}, _ptUsedRule = (["TestLanguage"],"op"), _ptUsedIndex = 1, ...},RuleEnter {_pt = RuleEnter {_pt = RuleEnter {_pt = Int {_ptInt = 7, ...}, _ptUsedRule = (["TestLanguage"],"int"), _ptUsedIndex = 0, ...}, _ptUsedRule = (["TestLanguage"],"expr"), _ptUsedIndex = 1, ...}, _ptUsedRule = (["TestLanguage"],"exprSum"), _ptUsedIndex = 1, ...}], ...}, _ptUsedRule = (["TestLanguage"],"exprSum"), _ptUsedIndex = 0, ...})])





>>> tNot arg	= resolveAndRun testLanguage (["TestLanguage"], "not") [createPT "expr" arg] & crash' & toParsable
>>> tNot "True"
"False"
>>> tNot "False"
"True"

>>> let tNand a b	= resolveAndRun testLanguage (["TestLanguage"], "nand") [createPT "expr" a, createPT "expr" b] & crash' & toParsable
>>> tNand "False" "False"
"True"
>>> tNand "True" "False"
"True"
>>> tNand "False" "True"
"True"
>>> tNand "True" "True"
"False"

>>> let tOr a b	= resolveAndRun testLanguage (["TestLanguage"], "or") [createPT "expr" a, createPT "expr" b] & crash' & toParsable
>>> tOr "True" "True"
"True"
>>> tOr "True" "False"
"True"
>>> tOr "False" "True"
"True"
>>> tOr "False" "False"
"False"

-}

type VariableStore' a
	= Map Name (ParseTree' a)
 
type VariableStore	= VariableStore' SyntFormIndex


-------------------------- ABOUT RUNNING A FUNCTION ------------------------------------



{- | Resolves the function, executes it. The first arguments adds an annotation to the parsetree, based on the type of the parsetree

-}
resolveAndRun	:: LDScope -> FQName -> [ParseTree] -> Failable ParseTree
resolveAndRun lds fqn@(targetLD, name) args
 | fqn `M.member` Builtins.functions
	= do	builtin	<- checkExists' fqn Builtins.functions "Wut? We just made sure this builtin function has this key?"
		builtin args
 | otherwise
	= do	ld	<- checkExistsSugg' dots targetLD (get environment lds) ("The module "++dots targetLD++" was not found")
		let ld'	= ld & get ldScope 	:: LanguageDef
		let msg	= "The module "++dots targetLD++" does not have a function section and thus does not define "++name
		funcs	<- ld' & get langFunctions 
				& maybe (fail msg) return

		func	<- checkExistsSuggDist' (show, levenshtein) name (get grouperDict funcs) 
				$ "The module "++dots targetLD++" does not define the function "++show name
		runFunction lds ld' func args 

runFunction	:: LDScope -> LanguageDef -> Function -> [ParseTree] -> Failable ParseTree
runFunction lds ld func args
	= inMsg' ("While executing the function "++get funcName func) $ inLocation (get (funcDoc . miLoc) func) $ 
		do	let clauses	= func & get funcClauses
			clauses & mapi |> flip (runClause lds ld) args & firstSuccess			


runClause	:: LDScope -> LanguageDef -> (Int, FunctionClause) -> [ParseTree] -> Failable ParseTree
runClause lds ld (i, FunctionClause pats result doc nm) args
	= inMsg' ("While trying clause "++ nm ++ "." ++ show i) $ inLocation (get miLoc doc) $
	  do	store	<- patternMatchAll lds M.empty pats args
		constructParseTree lds store result




------------------------- ABOUT PARSETREE CONSTRUCTION/INTERPRETATION --------------------------------



evalExpression		:: LDScope -> Expression -> Failable ParseTree
evalExpression langs
	= constructParseTree langs M.empty

constructParseTree	:: LDScope -> VariableStore -> Expression -> Failable ParseTree
constructParseTree _ vars (Var nm _ _)
	= checkExistsSuggDist' (show, levenshtein) nm vars ("No variable "++show nm++" is in scope here")
constructParseTree _ _ DontCare{}
	= fail "Found a wildcard (_) in an expression context. Only use these as patterns!"
constructParseTree _ _ (ParseTree pt b _)
	= do	let a	= b
		pt |> const a & return
constructParseTree lds vars (FuncCall func args _ _)
	= do	args'	<- args |+> constructParseTree lds vars
		resolveAndRun lds func args'
constructParseTree lds vars (Ascription expr expTp _ _)
	= do	pt	<- constructParseTree lds vars expr
		-- At first glance,  it might look as if the typechecker ensures this correctenss and ascriptions are useless
		-- However, ascription might be used as a predicate on rules, so this must be checked after all
		let actTp	= get (ptA . syntIndForm) pt
		let ld		= get ldScope lds
		assertSugg' (isSubtypeOf ld actTp expTp) 
			("Ascription failed: unexpected type "++showFQ actTp++" of value "++toParsable pt++
			 "\nThis value is constructed by the expresson "++toParsable expr
			, "Use an expression which results in a "++showFQ expTp++" instead")
		return pt
constructParseTree lds vars (Split exp1 exp2 _ _)
	= do	pt1	<- constructParseTree lds vars exp1
		pt2	<- constructParseTree lds vars exp2
		let pt1'	= bareInformation pt1		
		let pt2'	= bareInformation pt2
		assert' (pt1' == pt2') $ unlines
			["When a split is used in an expression context, both generated results should be the same"
			, "First result: "++toParsable pt1'
			,"Generated by: "++toParsable exp1
			, "Second result: "++toParsable pt2'
			, "Generated by: "++toParsable exp2]
		return pt1
constructParseTree lds vars (SeqExp exprs b _)
	= do	pts	<- exprs |> constructParseTree lds vars & allGood
		return $ Seq pts unknownLocation b

------------------------ ABOUT PATTERN MATCHING ---------------------------------------


patternMatchAll	:: LDScope -> VariableStore -> [Expression] -> [ParseTree]  -> Failable VariableStore
patternMatchAll lds store [] []
	= return store
patternMatchAll lds store (expr:exprs) (arg:args)
	= inLocation (get expLocation expr) $ do
		assert' (length exprs == length args)
			$ "Number of arguments is incorrect: got "++show (length args)++" patterns, but got "++show (length exprs)++" arguments"
		store'	<- patternMatch lds store expr arg
		mergedStores	<- mergeStores [store, store']
		patternMatchAll lds mergedStores exprs args






patternMatch	:: LDScope -> VariableStore -> Expression -> ParseTree -> Failable VariableStore
patternMatch _ _ (Var nm _ _) pt
	= return $ M.singleton nm pt
patternMatch _ _ (DontCare _ _) _
	= return M.empty
patternMatch _ _ (ParseTree expectedPT _ _) actPt
 	= do	let exp	= expectedPT & bareInformation
		let act	= actPt & bareInformation
		assert' (exp == act) $ unlines 
			["Argument does not match expected token"
			, "Expected: "++toParsable exp
			, "Got: "++toParsable act]
		return M.empty
patternMatch lds vars (FuncCall funcName args b _) arg
	= inMsg' ("While running a function within a pattern, namely "++showFQ funcName) $
	  do	argsPt		<- args |+> constructParseTree lds vars
		expectedPT	<- resolveAndRun lds funcName argsPt |> deAnnot
		patternMatch lds vars (ParseTree expectedPT b unknownLocation) arg
patternMatch lds vars (Ascription expr expType _ _) re@RuleEnter{}
	= do	let actType	= re & mostSpecificRuleEnter & get ptUsedRule
		assert' (isSubtypeOf (get ldScope lds) actType expType) $ unlines
			["Ascription failed"
			, "Expected something of type "++showFQ expType
			, "but got a parsetree of the form "++showFQ actType]
		patternMatch lds vars expr re
patternMatch lds vars (Split exp1 exp2 _ _) pt
	= do	store1	<- patternMatch lds vars exp1 pt
		vars'	<- mergeStores [store1, vars]
		store2	<- patternMatch lds vars' exp2 pt
		mergeStores [store1, store2]
patternMatch lds vars (SeqExp exprs _ _) (Seq pts _ _)
	= patternMatchAll lds vars exprs pts
patternMatch lds vars expr (RuleEnter pt _ _ _ _)
	= patternMatch lds vars expr pt

patternMatch _ _ expr pt
	= fail $ "Could not match "++toParsable expr++" with "++toParsable pt


mergeStores	:: [VariableStore' b] -> Failable (VariableStore' b)
mergeStores stores
 	= do	let stores'	= stores ||>> return & M.unionsWith sameBareInfo
		stores' & M.toList 
			|> sndEffect & allGood
			|> M.fromList


sameBareInfo	:: Failable (ParseTree' a) -> Failable (ParseTree' a) -> Failable (ParseTree' a)
sameBareInfo expectedPt actPt
	= do	exp	<- expectedPt |> bareInformation
		act	<- actPt |> bareInformation
		assert' (exp == act) $ unlines 
			["Variable store elements are not the same:"
			, "First instance: "++toParsable exp
			, "Other instance: "++toParsable act]
		actPt
