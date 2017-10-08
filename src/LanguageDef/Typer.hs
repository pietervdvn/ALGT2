module LanguageDef.Typer where

{- Types expressions and functions -}

import Utils.All

import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.Grouper
import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.Checkable

import LanguageDef.Data.BNF (BNF)
import qualified LanguageDef.Data.BNF as BNF
import LanguageDef.Data.ParseTree
import LanguageDef.Data.SyntacticForm

import LanguageDef.LangDefs

import LanguageDef.Data.LanguageDef
import LanguageDef.Data.Expression
import LanguageDef.Data.Function
import LanguageDef.Data.Rule
import LanguageDef.Data.Relation
import LanguageDef.Data.SyntFormIndex
import LanguageDef.MetaSyntax (typeTop, typeBottom)




import Graphs.Lattice

import Data.Either
import Data.Bifunctor (first)
import Control.Arrow ((&&&))

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List as L

import Control.Monad
import Data.Maybe

-------------------- TYPING OF A LANGUAGEDEF -----------------------------------

-- Types everything there is to type, knots the scopes afterwards
typeLD		:: Eq fr => Map [Name] (LDScope' fr) -> Failable (Map [Name] LDScope)
typeLD env
	= do	env'	<- env 	& M.toList 
				||>> typeScope'
				|> sndEffect & allGood 
				|> M.fromList
				:: Failable (Map [Name] LanguageDef)
		-- yes, this is a knot. fixedEnv is used in its own definition
		let fixedEnv	= M.intersectionWith (\lds ld -> updateEnv (ld, fixedEnv) lds) env env'	:: Map [Name] LDScope
		return fixedEnv


typeScope'	:: Eq fr => LDScope' fr	-> Failable LanguageDef
typeScope' ld
	= do	let langDef	= get ldScope ld
		langFuncs'	<- get langFunctions langDef & overGrouperMCmb' allGood (typeFunction ld)
		langRules'	<- get langRules langDef & overGrouperMCmb' allGood (typeRule ld)
		let langRules''	= langRules' |||>>> snd
		let langDef'	= updateFR (langFuncs', langRules'') langDef
		return langDef'		


-------------------- TYPING OF RULES/CONCLUSIONS/PREDICATES  ---------------------

{- | Types a rule

>>> import LanguageDef.API
>>> loadAssetLangDef "Faulty/Relations" ["TypeErr"] & toCoParsable
"| While typing rule \"not\" in Faulty/Relations/TypeErr.language at lines 21 - 25\n| While typing \"5\" while typing in Faulty/Relations/TypeErr.language at line 24, columns 5 - 8\n| Could not type the sequence \"5\" as a TypeErr.bool \n  Error: \n    \8226 Unexpected literal \"5\"\n    \8226 Expected literal \"True\"\n  Error: \n    \8226 Unexpected literal \"5\"\n    \8226 Expected literal \"False\""


-}
typeRule	:: Eq fr => LDScope' fr -> Rule' a -> Failable (Rule' (a, SyntFormIndex))
typeRule lds (Rule preds concl n mi)
	= inMsg' ("While typing rule "++show n) $ inLocation (get miLoc mi) $ 
	  do	preds'	<- preds |> typePredicate lds & allGood
		concl'	<- typeConclusion lds concl
		initialTable	<- _newlyCreatedVars lds concl'
		finalTable	<- foldM (typingTablePred lds) initialTable preds'
		checkAllVarsCompatible lds finalTable concl'

		return $ Rule preds' concl' n mi

{-

Given a current typing table, will check for a predicate that:
- All consumed variables are initialized
- All variable declarations are compatible

It will return a new typing table, eventually updated with new values

-}
typingTablePred		:: LDScope' fr -> Map Name SyntForm -> Predicate' (a, SyntFormIndex) -> Failable (Map Name SyntForm)
typingTablePred lds knownVars (PredExpr expr _)
	= do	_checkAllVarsExist knownVars expr
		checkVarsCompatible lds knownVars [typingTable' expr]
		return knownVars		 
typingTablePred lds knownVars (PredConcl concl _) 
	= do	-- In a predicate position, the inArgs should not contain unknown variables;
		inArgs	<- modedArgs lds In concl
		inArgs |> _checkAllVarsExist knownVars & allGood
		checkVarsCompatible lds knownVars (inArgs |> typingTable')

		-- the outArgs generate variables
		outArgs	<- modedArgs lds Out concl
		let knownVars'	= knownVars |> S.singleton
		let outArgs'	= outArgs |> typingTable'
		mergeTypings lds (knownVars' : outArgs')


-- Only to be used for the conclusion of the rule
_newlyCreatedVars	:: LDScope' fr -> Conclusion' (a, SyntFormIndex) -> Failable (Map Name SyntForm)
_newlyCreatedVars lds concl
	= do	inArgs	<- modedArgs lds In concl
		-- input arguments generate new variables that can be used
		let newVars	= inArgs ||>> snd |> typingTable
		mergeTypings lds newVars



{-
Checks that all the variables of the given conclusion, in the output argument, are declared. Only to be used for the conclusion of the rule.
Checks that each variable:
 - exists withing the given dict
 - have a compatible type
-}
checkAllVarsCompatible	:: LDScope' fr -> Map Name SyntForm -> Conclusion' (a, SyntFormIndex) -> Check
checkAllVarsCompatible lds knownVars concl
	= do	outArgs	<- modedArgs lds Out concl
		let exprTypings	= outArgs ||>> snd |> typingTable
		outArgs |> _checkAllVarsExist knownVars & allGood
		checkVarsCompatible lds knownVars exprTypings

checkVarsCompatible	:: LDScope' fr -> Map Name SyntForm -> [Map Name (Set SyntForm)] -> Check
checkVarsCompatible lds knownVars typingTables
	= do	let knownVars'	= knownVars |> S.singleton
		mergeTypings lds (knownVars' : typingTables)
		pass


_checkAllVarsExist	:: Map Name SyntForm -> Expression' (a, SyntFormIndex) -> Check
_checkAllVarsExist knownVars expr
	= inMsg' ("In the expression "++toParsable expr) $ inLocation (get expLocation expr) $
	  do	let exprTyping	= expr |> snd & typingTable
		let neededVars	= exprTyping & M.keys
		let missingVars	= neededVars & L.filter (`M.notMember` knownVars)
		unless (null missingVars) $
			multiFail (missingVars |> (\v -> "The variable "++show v++" is not in scope"))
		



modedArgs	:: LDScope' fr -> Mode -> Conclusion' a -> Failable [Expression' a]
modedArgs lds mode concl
	= let 	args	= concl & get conclArgs
		in
		selectModed lds concl mode args


selectModed	:: LDScope' fr -> Conclusion' x -> Mode -> [a] -> Failable [a]
selectModed lds concl mode as
	= inMsg' ("While selecting arguments with mode "++show mode++" for a conclusion about "++ showFQ (get conclRelName concl)) $
	  do	rel	<- get conclRelName concl
				& resolve' lds relationCall
				|> snd
		let modes	= get relTypes rel |> snd
		assertSugg' (length as == length modes) $ ("Unexpected number of arguments, namely "++show (length as), "Give "++show (length modes)++" arguments instead")
		zip modes as
			& filter ((==) mode . fst) 
			|> snd
			& return
		


typePredicate	:: Eq fr => LDScope' fr -> Predicate' a -> Failable (Predicate' (a, SyntFormIndex))
typePredicate lds (PredConcl concl li)
 	= do	concl'	<- typeConclusion lds concl
		PredConcl concl' li & return
typePredicate lds (PredExpr expr li)
	= do	expr'	<- typeExpressionFreely lds expr 
		PredExpr expr' li & return


typeConclusion	:: Eq fr => LDScope' fr -> Conclusion' a -> Failable (Conclusion' (a, SyntFormIndex))
typeConclusion lds (Conclusion rel args)
	= do	(relFQn, relation)	<- resolve' lds relationCall rel
		let types	= relation & get relTypes |> fst
		args'		<- zip types args |> uncurry (typeExpression lds)
					& allGood
		return $ Conclusion relFQn args'


------------------------------ TYPING OF FUNCTIONS  ------------------------------



typeFunction	:: Eq fr => LDScope' fr -> Function' x -> Failable Function
typeFunction ld f
 | "/src/Assets/TestLanguages/ALGT/Builtins.language" `isSuffixOf` (ld & get (ldScope . langLocation . miFile))
	= f & set funcClauses [] & return
 | otherwise
	= inMsg' ("While typing the function "++ show (get funcName f)) $
	  do	let clauses	= get funcClauses f
		let tps		= (get funcArgTypes f, get funcRetType f)
		clauses'	<- clauses & mapi |> typeClause ld tps & allGood
		f & set funcClauses clauses' & return
		
{- |

Typing of the clause. Checks for:

- Undefined variables
- Variable usage that is smaller then the possible input (e.g. f(x:expr) = intFunction(x))
- Conflicting usage

>>> import LanguageDef.API
>>> loadAssetLangDef "" ["Faulty","FunctionTyperTest"] & toCoParsable
"| While typing the function \"f\" \n| While typing clause f.0 in /Faulty/FunctionTyperTest.language at lines 25 - 26\n| The clause is: (f(x)\t = y) in /Faulty/FunctionTyperTest.language at lines 25 - 26\nError: \n  \8226 The variable \"y\" was not defined\n| While typing the function \"g\" \n| While typing clause g.0 in /Faulty/FunctionTyperTest.language at lines 28 - 29\n| The clause is: (g(x)\t = not(x)) in /Faulty/FunctionTyperTest.language at lines 28 - 29\n| While typing the return expression of the function, namely not(x) in /Faulty/FunctionTyperTest.language at line 28, columns 8 - 14\n| While typing not(x) while typing in /Faulty/FunctionTyperTest.language at line 28, columns 8 - 14\nError: \n  \8226 Unexpected type of 'not(x)', namely Faulty.FunctionTyperTest.bool\n  \8226 Use an expression which returns a Faulty.FunctionTyperTest.int (or a subset of that type)\n| While typing the function \"h\" \n  | While typing clause h.0 in /Faulty/FunctionTyperTest.language at lines 31 - 32\n  | The clause is: (h(x:int)\t = bool) in /Faulty/FunctionTyperTest.language at lines 31 - 32\n  Error: \n    \8226 The variable \"bool\" was not defined\n  | While typing clause h.1 in /Faulty/FunctionTyperTest.language at lines 32 - 33\n  | The clause is: (h(x)\t = not(x)) in /Faulty/FunctionTyperTest.language at lines 32 - 33\n  Error: \n    \8226   The variable \"x\" is used as a Faulty.FunctionTyperTest.bool, but could be a Faulty.FunctionTyperTest.expr which is broader"

-}

typeClause scope expectations (i, clause)
	=  inMsg' ("While typing clause "++get clauseFuncName clause ++"."++show i) $ inLocation (get (clauseDoc . miLoc) clause) $
		inMsg' ("The clause is: "++inParens (toParsable clause)) $
		_typeClause scope expectations (i, clause)

_typeClause	:: Eq fr => LDScope' fr -> ([FQName], FQName) -> (Int, FunctionClause' x) -> Failable FunctionClause
_typeClause scope (patExps, retExp) (i, FunctionClause pats ret doc nm)
 | length patExps /= length pats
	= inLocation (get miLoc doc) $ fail $ 
		"Expected "++show (length patExps)++" patterns for function "++show nm++", but got "++show (length pats)++" patterns instead"
 | otherwise
	= inLocation (get miLoc doc) $
	   do	pats'	<- zip patExps pats & mapi |> (\(i, (patExp, pat)) -> typePattern ("pattern "++show i) scope patExp pat)
				& allGood
		ret'	<- typePattern "the return expression of the function" scope retExp ret	-- typing of the main return expression

		-- Patterns, such as `f((x:expr), (x:int))` will get the intersection
		patternTypes	<- inMsg' "While checking for conflicting variable usage between patterns" $
					mergeTypings scope (pats' |> typingTable)
		-- The usage in the return expression should be smaller, but never bigger: `f((x:expr)) = !plus((x:int), 5)` implies a type error
		exprTypes	<- inMsg' "While checking for conflicting variable in the clause body" $ 
					mergeTypings scope [typingTable ret']
		inMsg' "While checking for conflicting variable usage between patterns and the clause body" $ 
			mergeTypings scope ((ret':pats') |> typingTable)
		
		-- check that each variable exists
		exprTypes & M.keys & filter (not . (`M.member` patternTypes))
			|> (\k -> "The variable "++show k++" was not defined")
			|> fail & allGood

		let supertypings	= scope & get (ldScope . langSupertypes)
		-- check that usage of each variable is a supertype of what the patterns generate (what a pattern gives is a subtype of what is needed)
		let notSubset	= M.intersectionWith (,) patternTypes exprTypes	
					& M.filter (uncurry (/=))
					& M.filter (\(decl, usage) -> not $ isSubsetOf supertypings decl usage) :: Map Name (SyntForm, SyntForm)
		let notSubsetMsg (nm, (patT, exprT))
				= "The variable "++show nm++" is used as a "++showFQ exprT++", but could be a "++showFQ patT++" which is broader"
		assert' (null notSubset) (notSubset & M.toList |> notSubsetMsg & unlines & indent)

		return $ FunctionClause pats' ret' doc nm



typePattern	:: Eq fr => String -> LDScope' fr -> FQName -> Expression' x  -> Failable Expression
typePattern msg ld exp pat
	= inMsg' ("While typing "++msg++", namely "++toParsable pat) $ inLocation (get expLocation pat) $ do
		pat'	<- typeExpression ld exp pat ||>> snd
		mergeTypings ld [typingTable pat']
		return pat'



------------------------------ TYPING OF EXPRESSIONS  ------------------------------


{- | Same as 'typeExpression', but does not require a type expectation 
>>> import LanguageDef.API
>>> let expr	=  createExpression testLanguage "interactive" "\"True\"" & crash'
>>> typeExpressionFreely testLanguage expr & crash'
ParseTree {_expPT = Literal {_ptToken = "True", _ptLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}, _ptA = (), _ptHidden = False}, _expAnnot = ((),SyntFormIndex {_syntIndForm = (["TestLanguage"],"bool"), _syntIndChoice = 0, _syntIndSeqInd = Just 0}), _expLocation = LocationInfo {_liStartLine = 0, _liEndLine = 0, _liStartColumn = 0, _liEndColumn = 6, _miFile = "interactive"}}

-}
typeExpressionFreely	:: Eq fr => LDScope' fr -> Expression' a -> Failable (Expression' (a, SyntFormIndex))
typeExpressionFreely lds
	= typeExpression lds typeTop




{- | Types an expression to the given expectation (and fully qualifies all calls and types in the mean time)

>>> import LanguageDef.API

>>> typeExpression (error "") (["A"], "b") (DontCare () unknownLocation)
Success (DontCare {_expAnnot = ((),NoIndex {_syntIndForm = (["A"],"b")}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}})
>>> typeExpression testLanguage (["TestLanguage"], "bool") (Var "x" () unknownLocation)
Success (Var {_varName = "x", _expAnnot = ((),NoIndex {_syntIndForm = (["TestLanguage"],"bool")}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}})
>>> typeExpression testLanguage (["TestLanguage"], "bool") (ParseTree (simplePT "True") () unknownLocation)
Success (ParseTree {_expPT = Literal {_ptToken = "True", _ptLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}, _ptA = (), _ptHidden = False}, _expAnnot = ((),SyntFormIndex {_syntIndForm = (["TestLanguage"],"bool"), _syntIndChoice = 0, _syntIndSeqInd = Just 0}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}})
>>> typeExpression testLanguage (["TestLanguage"], "bool") (Split (ParseTree (simplePT "True") () unknownLocation) (DontCare () unknownLocation) () unknownLocation)
Success (Split {_exp1 = ParseTree {_expPT = Literal {_ptToken = "True", _ptLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}, _ptA = (), _ptHidden = False}, _expAnnot = ((),SyntFormIndex {_syntIndForm = (["TestLanguage"],"bool"), _syntIndChoice = 0, _syntIndSeqInd = Just 0}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}}, _exp2 = DontCare {_expAnnot = ((),NoIndex {_syntIndForm = (["TestLanguage"],"bool")}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}}, _expAnnot = ((),NoIndex {_syntIndForm = (["TestLanguage"],"bool")}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}})
 -}
typeExpression	:: Eq fr => LDScope' fr -> SyntForm -> Expression' a -> Failable (Expression' (a, SyntFormIndex)) 
typeExpression lds expectation expr
	= inContext ("While typing "++toParsable expr, Typing, get expLocation expr) $ 
		_typeExpression lds expectation expr


_typeExpression	:: Eq fr => LDScope' fr -> SyntForm -> Expression' a -> Failable (Expression' (a, SyntFormIndex)) 
_typeExpression _ expectation (Var nm a li)
	= return $ Var nm (a, NoIndex expectation) li
_typeExpression _ expectation  (DontCare a li)
	= return $ DontCare (a, NoIndex expectation) li
_typeExpression ld expectation expr'@(FuncCall funcNm args a li)
	= do	(fqname, function)	<- resolve' ld functionCall funcNm
		argTypes		<- get funcArgTypes function |> resolve ld syntaxCall & allGood
		args'			<- zip argTypes args |+> uncurry (typeExpression ld)

		let argSugg	= "Give "++ show (length argTypes) ++" arguments, of types "++(argTypes |> showFQ & commas)
		assertSugg' (length argTypes >= length args)
			("Too little arguments given, namely "++show (length args), argSugg)
		assertSugg' (length argTypes <= length args)
			("Too much arguments given, namely "++show (length args), argSugg)


		let ld'			= get ldScope ld
		ftype	<- get funcRetType function & resolve ld syntaxCall
		assertSugg' (isSubtypeOf ld' ftype expectation || isSubtypeOf ld' expectation ftype)
			("Unexpected type of '"++ toParsable expr' ++"', namely "++ showFQ ftype, "Use an expression which returns a "++showFQ expectation++" (or a subset of that type)")
		return (FuncCall fqname args' (a, NoIndex ftype) li)
_typeExpression ld expectation (Ascription expr name a li)
	= do	name'	<- resolve ld syntaxCall name
		expr'	<- typeExpression ld name' expr
		assert' (isSubtypeOf (get ldScope ld) name' expectation)
			$ "The ascription of "++toParsable expr++" as a "++showFQ name++" is not a subtype of "++showFQ expectation
		return $ Ascription expr' name' (a, NoIndex name') li
_typeExpression ld expectation (Split e1 e2 a li)
	= do	e1'	<- typeExpression ld expectation e1
		e2'	<- typeExpression ld expectation e2
		let t e	= get expAnnot e & snd & removeIndex'
		let t1	= t e1'
		let t2	= t e2'
		let msg	e	= toParsable e ++ "\t: " ++toParsable (t e)
		assert' (t1 == t2) $ "Types of arguments in a split should be the same, but the types are different:" ++
			indent (unlines [msg e1', msg e2'])
		return $ Split e1' e2' (a, t1) li
_typeExpression ld expectation pt@ParseTree{}
	= do	(expr', _)	<- typeExpressionIndexed ld expectation [pt]
		assert' (length expr' == 1) "Huh, this is weird. Bug in typeExpression: multiple possible expressions for a parsetree"
		return $ head expr'
_typeExpression ld expectation (SeqExp exprs a li)
	= do	(exprs', subtype)	<- typeExpressionIndexed ld expectation exprs
		return $ SeqExp exprs' (a, subtype) li

{-Search for a choice from the syntactic forms matching the expressions -}
typeExpressionIndexed	:: Eq fr => LDScope' fr -> SyntForm -> [Expression' a] -> Failable ([Expression' (a, SyntFormIndex)], SyntFormIndex)
typeExpressionIndexed ld syntForm exprs
 | syntForm == typeTop
	= do	let all		= allKnowns ld syntaxCall |> fst3 |> fst & filter (`notElem` [typeTop, typeBottom])	:: [FQName]
		let tries	= all |> (\expectation -> typeExpressionIndexed ld expectation exprs)
		let isSubtypeOf'	= isSubtypeOf (get ldScope ld)
		let successfull	= successess tries & selectSmallest isSubtypeOf' -- [([Expression' (a, SyntFormIndex)], SyntFormIndex)]
		inMsg' ("Could not type the sequence "++ unwords (exprs |> toParsable)++" as any known type") $
			inMsg' ("Tried the types: "++all |> showFQ & commas' "and") $
			when (null successfull) (fails tries & Aggregate & Failed)

		assertSugg' (length successfull < 2) $ ("The sequence "++ unwords (exprs |> toParsable)++" could be typed in multiple ways, namely as:\n"
			++ (successfull |> (\(_, sfi) -> "Via choice "++toParsable sfi) & unlines & indent), "Try adding a type ascription to disambiguate the typing")
		let found	= head successfull
		return found
		
 | otherwise
	= do	(fqname, choices)	<- inMsg' ("While typing "++ exprs |> toParsable & commas) 
						$ resolve' ld syntaxCall syntForm ||>> get syntChoices
		let tries	= choices |> BNF.removeWS |> BNF.unsequence	-- prepare individual choices
					& mapi 					-- Number the choices
					|> typeExpressionIndexed' ld syntForm exprs	-- Actually type them
					& mapi |> sndEffect				-- and number it again
		let successfull	= successess tries		--	:: [(Int, [Expression' (a, SyntFormIndex)])]
		let li		= exprs & head & get expLocation
		when (null successfull) $ 
			fails tries & Aggregate & Failed & inMsg' ("Could not type the sequence "++ unwords (exprs |> toParsable)++" as a "++showFQ syntForm)

		assertSugg' (length successfull < 2) $ ("The sequence "++ unwords (exprs |> toParsable)++" could be typed in multiple ways, namely as:\n"
			++ (successfull |> (\(i, exprs) -> "Via choice "++show i) & unlines & indent), "Try adding a type ascription to disambiguate the typing")
		let (foundInd, foundExprs)	= head successfull
		(foundExprs, SyntFormIndex fqname foundInd Nothing) & return


typeExpressionIndexed'	:: Eq fr => LDScope' fr -> SyntForm -> [Expression' a] -> (Int, [BNF]) -> Failable [Expression' (a, SyntFormIndex)]
typeExpressionIndexed' ld syntForm exprs (choiceIndex, choiceElems)
 | length choiceElems == 1 && length exprs /= 1 && BNF.isRuleCall (head choiceElems) 
	= do	let [BNF.RuleCall fqname]	= choiceElems
		typeExpressionIndexed ld fqname exprs |> fst 
 | length exprs /= length choiceElems
	= fail $"Can not match choice "++show choiceIndex++": this choice has "++show (length choiceElems) ++ 
		" elements, whereas the expression has "++show (length exprs)
 | otherwise
	= zip (mapi choiceElems) exprs |> uncurry (typeExprBNF ld (syntForm, choiceIndex)) & allGood


typeExprBNF		:: Eq fr => LDScope' fr -> (SyntForm, Int) -> (Int, BNF) -> Expression' a -> Failable (Expression' (a, SyntFormIndex))
typeExprBNF ldscope (form, choiceInd) (seqIndex, BNF.RuleCall fqname) expr
	= typeExpression ldscope fqname expr
typeExprBNF ldscope (form, choiceInd) (seqIndex, bnf) expr
	-- At this point, the bnf can not be: RuleCall (prev. case), Seq (BNF.unsequence was run in typeExpressionIndexed)
	-- Still resting: Literal, Builtin, Group -- which all should have a matching parsetree (or dontcare)
	= let	fi	= SyntFormIndex form choiceInd (Just seqIndex) in 
	  case expr of
		(ParseTree pt a li)	-> _compareBNFPT bnf pt >> return (ParseTree pt (a, fi) li)
		(DontCare a li)		-> return (DontCare (a, fi) li)
		(Var nm a li)		-> return (Var nm (a, fi) li)
		a@Ascription{}		-> fail $
						"Found ascription "++toParsable a++" where a literal value of the form "++toParsable bnf++
						" was expected; ascriptions can only match rulecalls"
		s@SeqExp{}		-> fail $
						"Found a sequence "++toParsable s++" where a literal value of the form "++toParsable bnf++
						" was expected; sequences can only match rulecalls"
		fc@FuncCall{}		-> fail $ "Found a function call "++toParsable fc++" where a literal value of the form "++toParsable bnf++
						" was expected; functions can only match rulecalls"
		ft			-> error $ "BUG: Fallthrough!: " ++ debug  ft


_compareBNFPT	:: BNF -> ParseTree' a -> Failable ()
_compareBNFPT (BNF.Literal str) (Literal token li _ _)
	= assertSugg' (str == token) ("Unexpected literal "++show token, "Expected literal "++show str)
_compareBNFPT (BNF.Literal str) (Int token li _)
	= assertSugg' (str == show token) ("Unexpected int "++show token, "Expected literal "++show str)
_compareBNFPT (BNF.BuiltIn _ builtin) (Literal token li _ _)
	= assertSugg' (token `BNF.isElementOf` builtin) ("Unexpected "++show token, "Expected a "++get BNF.biName builtin)
_compareBNFPT (BNF.BuiltIn _ builtin) (Int token li _)
 	= assertSugg' (builtin `elem` [BNF.intBI, BNF.numberBI]) ("Unexpected int "++show token, "Expected a "++get BNF.biName builtin)
_compareBNFPT (BNF.RuleCall name) re@RuleEnter{}
	= let	actRule = get ptUsedRule re in
		assertSugg' (name == actRule)
			("Unexpected parsetree contstructed with "++ showFQ actRule++": "++toParsable re
			, "Expected a parsetree constructed with the syntactic form "++showFQ name)
_compareBNFPT (BNF.Group bnf) pt
	= _compareBNFPT bnf pt
_compareBNFPT (BNF.Seq bnf) pt
	= fail "Bug: sequence should not be reachable here"



{- | Maps an expression onto all possible types it assumes
>>> typingTable $ DontCare {_expAnnot = (NoIndex (["A"],"b")), _expLocation = unknownLocation}
fromList []
>>> let varX = Var {_varName = "x", _expAnnot = (NoIndex (["TestLanguage"],"bool")), _expLocation = unknownLocation}
>>> typingTable varX
fromList [("x",fromList [(["TestLanguage"],"bool")])]
>>> let varInt = Var {_varName = "x", _expAnnot = (NoIndex (["TestLanguage"],"int")), _expLocation = unknownLocation}
>>> let seq = SeqExp [varX, varInt] (error "No index here") unknownLocation
>>> typingTable seq
fromList [("x",fromList [(["TestLanguage"],"bool"),(["TestLanguage"],"int")])]

-}
typingTable	:: Expression -> Map Name (Set FQName)
typingTable (Var nm (NoIndex sf) _)
	= M.singleton nm (S.singleton sf)
typingTable DontCare{}
	= M.empty
typingTable ParseTree{}
	= M.empty
typingTable (FuncCall _ args _ _)
	= args |> typingTable & M.unionsWith S.union
typingTable (Ascription expr _ _ _)
	= typingTable expr
typingTable (SeqExp exprs _ _)
	= exprs |> typingTable & M.unionsWith S.union

typingTable'	:: Expression' (a, SyntFormIndex) -> Map Name (Set FQName)
typingTable' e
	= typingTable (e |> snd)


{- | checks that no two typings do conflict (thus that no two typings result in an empty set for the variables). The typingtable is used for this

>>> import LanguageDef.API
>>> let (Success boolVar) = typeExpression testLanguage (["TestLanguage"], "bool") (Var "x" () unknownLocation)
>>> boolVar
Var {_varName = "x", _expAnnot = ((),NoIndex {_syntIndForm = (["TestLanguage"],"bool")}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}}
>>> let (Success intVar)  = typeExpression testLanguage (["TestLanguage"], "int") (Var "x" () unknownLocation)
>>> intVar
Var {_varName = "x", _expAnnot = ((),NoIndex {_syntIndForm = (["TestLanguage"],"int")}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}}
>>> [boolVar, intVar] |> typingTable' & mergeTypings testLanguage & handleFailure toCoParsable show
"| While checking for conflicting variable usage \nError: \n  \8226 x\tis used as TestLanguage.bool, TestLanguage.int"

-}
mergeTypings	:: LDScope' fr -> [Map Name (Set SyntForm)] -> Failable (Map Name SyntForm)
mergeTypings lds typings
	= inMsg' "While checking for conflicting variable usage" $
	  do	let lattice	= get (ldScope . langSupertypes) lds
		let typings'	= typings
					& M.unionsWith S.union
					-- search a common subtype for each set: {"a" --> ⊥, "b" --> Bool}
					|> (id &&& infimums lattice)	
		let failedTypings
				= typings' & M.filter ((==) (get bottom lattice) . snd)	-- entries which are bottom (⊥) have failed typing
					|> fst
					& M.toList
		let	showEntry (nm, tps)	= nm++"\tis used as "++ (tps & S.toList |> showFQ & commas)
		unless (null failedTypings) (failedTypings |> showEntry & multiFail) 
		return (typings' |> snd)
