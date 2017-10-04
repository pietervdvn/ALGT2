module LanguageDef.Typer where

{- Types expressions and functions -}

import Utils.All

import LanguageDef.LanguageDef
import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.Grouper
import LanguageDef.Utils.Scope
import LanguageDef.Utils.ExceptionInfo

import LanguageDef.Syntax.BNF (BNF)
import qualified LanguageDef.Syntax.BNF as BNF
import LanguageDef.Syntax hiding (assert')
import LanguageDef.LangDefs

import LanguageDef.Expression
import LanguageDef.Function
import LanguageDef.Rule
import LanguageDef.Relation


import Graphs.Lattice

import Data.Either
import Data.Bifunctor (first)
import Control.Arrow ((&&&))

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad
import Data.Maybe

-------------------- TYPING OF A LANGUAGEDEF -----------------------------------

typeLD		:: Eq fr => Map [Name] (LDScope' fr) -> Failable (Map [Name] LDScope)
typeLD lds
	= do	typed	<- lds & M.toList ||>> typeScope |> sndEffect & allGood |> M.fromList
				:: Failable (Map [Name] (Scope [Name] [Name] LanguageDef ImportFlags ()))
		let env	= typed |> get payload
		let scopes	= typed |> flip LDScope env	:: Map [Name] LDScope
		return scopes



typeScope	:: Eq fr =>  LDScope' fr -> Failable (Scope [Name] [Name] LanguageDef ImportFlags ())
typeScope scope
	= do	let langDef	= get (ldScope . payload) scope
		langDef'	<- typeScope' scope langDef
		let scope'	= scope & get ldScope & set payload langDef'
		return scope'


typeScope'	:: Eq fr => LDScope' fr -> LanguageDef' ResolvedImport fr
			-> Failable (LanguageDef' ResolvedImport SyntFormIndex)
typeScope' ld langDef
	= do	langFuncs'	<- get langFunctions langDef & overGrouperMCmb' allGood (typeFunction ld)
		langRules'	<- get langRules langDef & overGrouperMCmb' allGood (typeRule ld)
		let langRules''	= langRules' |||>>> snd
		return $ updateFR (langFuncs', langRules'') langDef
		


-------------------- TYPING OF RULES/CONCLUSIONS/PREDICATES  ---------------------

{- | Types a rule

>>> import LanguageDef.API
>>> loadAssetLangDef "Faulty/Relations" ["TypeErr"] & toCoParsable
"| While typing \"5\" while typing in Faulty/Relations/TypeErr.language at line 24, columns 5 - 8\n| Could not type the sequence \"5\" as a TypeErr.bool \n  Error: \n    \8226 Unexpected literal \"5\"\n    \8226 Expected literal \"True\"\n  Error: \n    \8226 Unexpected literal \"5\"\n    \8226 Expected literal \"False\""


-}
typeRule	:: Eq fr => LDScope' fr -> Rule' a -> Failable (Rule' (a, SyntFormIndex))
typeRule lds (Rule preds concl n mi)
	= do	preds'	<- preds |> typePredicate lds & allGood
		concl'	<- typeConclusion lds concl
		-- TODO crosscheck variables for common ground
		-- TODO check introduction correctness from left to right
		return $ Rule preds' concl' n mi


typePredicate	:: Eq fr => LDScope' fr -> Predicate a -> Failable (Predicate (a, SyntFormIndex))
typePredicate lds (Left concl)
 	= do	concl'	<- typeConclusion lds concl
		concl' & Left & return
typePredicate lds (Right expr)
	= do	expr'	<- typeExpressionFreely lds expr 
		expr' & Right & return


typeConclusion	:: Eq fr => LDScope' fr -> Conclusion a -> Failable (Conclusion (a, SyntFormIndex))
typeConclusion lds (Conclusion rel args)
	= do	(relFQn, relation)	<- resolve' lds relationCall rel
		let types	= relation & get relTypes |> fst
		args'		<- zip types args |> uncurry (typeExpression lds)
					& allGood
		return $ Conclusion relFQn args'


------------------------------ TYPING OF FUNCTIONS  ------------------------------



typeFunction	:: Eq fr => LDScope' fr -> Function' x -> Failable Function
typeFunction ld f
	= inMsg' ("While typing "++ get funcName f) $
	  do	let clauses	= get funcClauses f
		let tps		= (get funcArgTypes f, get funcRetType f)
		clauses'	<- clauses & mapi |> typeClause ld (ld & get (ldScope . payload . langSupertypes)) tps & allGood
		f & set funcClauses clauses' & return
		
{- |

Typing of the clause. Checks for:

- Undefined variables
- Variable usage that is smaller then the possible input (e.g. f(x:expr) = intFunction(x))
- Conflicting usage

>>> import LanguageDef.API
>>> loadAssetLangDef "" ["Faulty","FunctionTyperTest"] & toCoParsable
"| While typing f \n| While typing clause f.0 in /Faulty/FunctionTyperTest.language at lines 25 - 26\n| The clause is: (f(x)\t = y) \nError: \n  \8226 The variable \"y\" was not defined\n| While typing g \n| While typing clause g.0 in /Faulty/FunctionTyperTest.language at lines 28 - 29\n| The clause is: (g(x)\t = not(x)) \n| While typing the return expression of the function, namely not(x) \n| While typing not(x) while typing in /Faulty/FunctionTyperTest.language at line 28, columns 8 - 14\nError: \n  \8226 Unexpected type of 'not(x)', namely Faulty.FunctionTyperTest.bool\n  \8226 Use an expression which returns a Faulty.FunctionTyperTest.int (or a subset of that type)\n| While typing h \n  | While typing clause h.0 in /Faulty/FunctionTyperTest.language at lines 31 - 32\n  | The clause is: (h(x:int)\t = bool) \n  Error: \n    \8226 The variable \"bool\" was not defined\n  | While typing clause h.1 in /Faulty/FunctionTyperTest.language at lines 32 - 33\n  | The clause is: (h(x)\t = not(x)) \n  Error: \n    \8226   The variable \"x\" is used as a Faulty.FunctionTyperTest.bool, but could be a Faulty.FunctionTyperTest.expr which is broader"

-}

typeClause scope supers expectations (i, clause)
	=  inMsg' ("While typing clause "++get clauseFuncName clause ++"."++show i) $ inLocation (get (clauseDoc . miLoc) clause) $
		inMsg' ("The clause is: "++inParens (toParsable clause)) $
		_typeClause scope supers expectations (i, clause)

_typeClause	:: Eq fr => LDScope' fr -> Lattice FQName -> ([FQName], FQName) -> (Int, FunctionClause x) -> Failable (FunctionClause SyntFormIndex)
_typeClause scope supertypings (patExps, retExp) (i, FunctionClause pats ret doc nm)
 | length patExps /= length pats
	= inLocation (get miLoc doc) $ fail $ 
		"Expected "++show (length patExps)++" patterns for function "++show nm++", but got "++show (length pats)++" patterns instead"
 | otherwise
	= do	let li	= get miLoc doc
		pats'	<- zip patExps pats & mapi |> (\(i, (patExp, pat)) -> typePattern ("pattern "++show i) scope supertypings patExp pat)
				& allGood
		ret'	<- typePattern "the return expression of the function" scope supertypings retExp ret	-- typing of the main return expression

		-- Patterns, such as `f((x:expr), (x:int))` will get the intersection
		patternTypes	<- inMsg' "While checking for conflicting variable usage between patterns" $
					mergeTypings' li supertypings pats'
		-- The usage in the return expression should be smaller, but never bigger: `f((x:expr)) = !plus((x:int), 5)` implies a type error
		exprTypes	<- inMsg' "While checking for conflicting variable in the clause body" $ 
					mergeTypings' li supertypings [ret']
		inMsg' "While checking for conflicting variable usage between patterns and the clause body" $ 
			mergeTypings' li  supertypings (ret':pats')
		
		-- check that each variable exists
		exprTypes & M.keys & filter (not . (`M.member` patternTypes))
			|> (\k -> "The variable "++show k++" was not defined")
			|> fail & allGood

		-- check that usage of each variable is a supertype of what the patterns generate (what a pattern gives is a subtype of what is needed)
		let notSubset	= M.intersectionWith (,) patternTypes exprTypes	
					& M.filter (uncurry (/=))
					& M.filter (\(decl, usage) -> not $ isSubsetOf supertypings decl usage) :: Map Name (SyntForm, SyntForm)
		let notSubsetMsg (nm, (patT, exprT))
				= "The variable "++show nm++" is used as a "++showFQ exprT++", but could be a "++showFQ patT++" which is broader"
		assert' (null notSubset) (notSubset & M.toList |> notSubsetMsg & unlines & indent)

		return $ FunctionClause pats' ret' doc nm



typePattern	:: Eq fr => String -> LDScope' fr -> Lattice FQName -> FQName -> Expression x  -> Failable (Expression SyntFormIndex)
typePattern msg ld supers exp pat
	= inMsg' ("While typing "++msg++", namely "++toParsable pat) $ do
		pat'	<- typeExpression ld exp pat ||>> snd
		mergeTypings' (get expLocation pat) supers [pat']
		return pat'



------------------------------ TYPING OF EXPRESSIONS  ------------------------------


{- | Same as 'typeExpression', but does not require a type expectation 
>>> import LanguageDef.API
>>> let expr	=  createExpression testLanguage "interactive" "\"True\"" & crash'
>>> typeExpressionFreely testLanguage' expr & crash'
ParseTree {_expPT = Literal {_ptToken = "True", _ptLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}, _ptA = (), _ptHidden = False}, _expAnnot = ((),SyntFormIndex {_syntIndForm = (["TestLanguage"],"bool"), _syntIndChoice = 0, _syntIndSeqInd = Just 0}), _expLocation = LocationInfo {_liStartLine = 0, _liEndLine = 0, _liStartColumn = 0, _liEndColumn = 6, _miFile = "interactive"}}

-}
typeExpressionFreely	:: Eq fr => LDScope' fr -> Expression a -> Failable (Expression (a, SyntFormIndex))
typeExpressionFreely lds
	= typeExpression lds typeTop




{- | Types an expression to the given expectation (and fully qualifies all calls and types in the mean time)

>>> import LanguageDef.API

>>> typeExpression (error "") (["A"], "b") (DontCare () unknownLocation)
Success (DontCare {_expAnnot = ((),NoIndex {_syntIndForm = (["A"],"b")}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}})
>>> typeExpression testLanguage' (["TestLanguage"], "bool") (Var "x" () unknownLocation)
Success (Var {_varName = "x", _expAnnot = ((),NoIndex {_syntIndForm = (["TestLanguage"],"bool")}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}})
>>> typeExpression testLanguage' (["TestLanguage"], "bool") (ParseTree (simplePT "True") () unknownLocation)
Success (ParseTree {_expPT = Literal {_ptToken = "True", _ptLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}, _ptA = (), _ptHidden = False}, _expAnnot = ((),SyntFormIndex {_syntIndForm = (["TestLanguage"],"bool"), _syntIndChoice = 0, _syntIndSeqInd = Just 0}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}})
>>> typeExpression testLanguage' (["TestLanguage"], "bool") (Split (ParseTree (simplePT "True") () unknownLocation) (DontCare () unknownLocation) () unknownLocation)
Success (Split {_exp1 = ParseTree {_expPT = Literal {_ptToken = "True", _ptLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}, _ptA = (), _ptHidden = False}, _expAnnot = ((),SyntFormIndex {_syntIndForm = (["TestLanguage"],"bool"), _syntIndChoice = 0, _syntIndSeqInd = Just 0}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}}, _exp2 = DontCare {_expAnnot = ((),NoIndex {_syntIndForm = (["TestLanguage"],"bool")}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}}, _expAnnot = ((),NoIndex {_syntIndForm = (["TestLanguage"],"bool")}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}})
 -}
typeExpression	:: Eq fr => LDScope' fr -> SyntForm -> Expression a -> Failable (Expression (a, SyntFormIndex)) 
typeExpression lds expectation expr
	= inContext ("While typing "++toParsable expr, Typing, get expLocation expr) $ 
		_typeExpression lds expectation expr


_typeExpression	:: Eq fr => LDScope' fr -> SyntForm -> Expression a -> Failable (Expression (a, SyntFormIndex)) 
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


		let ld'			= get (ldScope . payload) ld
		ftype	<- get funcRetType function & resolve ld syntaxCall
		assertSugg' (isSubtypeOf ld' ftype expectation || isSubtypeOf ld' expectation ftype)
			("Unexpected type of '"++ toParsable expr' ++"', namely "++ showFQ ftype, "Use an expression which returns a "++showFQ expectation++" (or a subset of that type)")
		return (FuncCall fqname args' (a, NoIndex ftype) li)
_typeExpression ld expectation (Ascription expr name a li)
	= do	name'	<- resolve ld syntaxCall name
		expr'	<- typeExpression ld name' expr
		assert' (isSubtypeOf (get (ldScope . payload) ld) name' expectation)
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
typeExpressionIndexed	:: Eq fr => LDScope' fr -> SyntForm -> [Expression a] -> Failable ([Expression (a, SyntFormIndex)], SyntFormIndex)
typeExpressionIndexed ld syntForm exprs
 | syntForm == typeTop
	= do	let all		= allKnowns ld syntaxCall |> fst3 |> fst	:: [FQName]
		let tries	= all |> (\expectation -> typeExpressionIndexed ld expectation exprs)
		let isSubtypeOf'	= isSubtypeOf (get (ldScope . payload) ld)
		let successfull	= successess tries & selectSmallest isSubtypeOf' -- [([Expression (a, SyntFormIndex)], SyntFormIndex)]
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
		let successfull	= successess tries		--	:: [(Int, [Expression (a, SyntFormIndex)])]
		let li		= exprs & head & get expLocation
		when (null successfull) $ 
			fails tries & Aggregate & Failed & inMsg' ("Could not type the sequence "++ unwords (exprs |> toParsable)++" as a "++showFQ syntForm)

		assertSugg' (length successfull < 2) $ ("The sequence "++ unwords (exprs |> toParsable)++" could be typed in multiple ways, namely as:\n"
			++ (successfull |> (\(i, exprs) -> "Via choice "++show i) & unlines & indent), "Try adding a type ascription to disambiguate the typing")
		let (foundInd, foundExprs)	= head successfull
		(foundExprs, SyntFormIndex fqname foundInd Nothing) & return


typeExpressionIndexed'	:: Eq fr => LDScope' fr -> SyntForm -> [Expression a] -> (Int, [BNF]) -> Failable [Expression (a, SyntFormIndex)]
typeExpressionIndexed' ld syntForm exprs (choiceIndex, choiceElems)
 | length choiceElems == 1 && length exprs /= 1 && BNF.isRuleCall (head choiceElems) 
	= do	let [BNF.RuleCall fqname]	= choiceElems
		typeExpressionIndexed ld fqname exprs |> fst 
 | length exprs /= length choiceElems
	= fail $"Can not match choice "++show choiceIndex++": this choice has "++show (length choiceElems) ++ 
		" elements, whereas the expression has "++show (length exprs)
 | otherwise
	= zip (mapi choiceElems) exprs |> uncurry (typeExprBNF ld (syntForm, choiceIndex)) & allGood


typeExprBNF		:: Eq fr => LDScope' fr -> (SyntForm, Int) -> (Int, BNF) -> Expression a -> Failable (Expression (a, SyntFormIndex))
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
						" was expected; ascriptions can only match rulecalls"

_compareBNFPT	:: BNF -> ParseTree a -> Failable ()
_compareBNFPT (BNF.Literal str) (Literal token li _ _)
	= assertSugg' (str == token) ("Unexpected literal "++show token, "Expected literal "++show str)
_compareBNFPT (BNF.Literal str) (Int token li _)
	= assertSugg' (str == show token) ("Unexpected int "++show token, "Expected literal "++show str)
_compareBNFPT (BNF.BuiltIn _ builtin) (Literal token li _ _)
	= assertSugg' (token `BNF.isElementOf` builtin) ("Unexpected "++show token, "Expected a "++get BNF.biName builtin)
_compareBNFPT (BNF.BuiltIn _ builtin) (Int token li _)
 	= assertSugg' (builtin `elem` [BNF.intBI, BNF.numberBI]) ("Unexpected int "++show int, "Expected a "++get BNF.biName builtin)
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
>>> _typingTable $ DontCare {_expAnnot = (NoIndex (["A"],"b")), _expLocation = unknownLocation}
fromList []
>>> let varX = Var {_varName = "x", _expAnnot = (NoIndex (["TestLanguage"],"bool")), _expLocation = unknownLocation}
>>> _typingTable varX
fromList [("x",fromList [(["TestLanguage"],"bool")])]
>>> let varInt = Var {_varName = "x", _expAnnot = (NoIndex (["TestLanguage"],"int")), _expLocation = unknownLocation}
>>> let seq = SeqExp [varX, varInt] (error "No index here") unknownLocation
>>> _typingTable seq
fromList [("x",fromList [(["TestLanguage"],"bool"),(["TestLanguage"],"int")])]

-}
_typingTable	:: Expression SyntFormIndex -> Map Name (Set FQName)
_typingTable (Var nm (NoIndex sf) _)
	= M.singleton nm (S.singleton sf)
_typingTable DontCare{}
	= M.empty
_typingTable ParseTree{}
	= M.empty
_typingTable (FuncCall _ args _ _)
	= args |> _typingTable & M.unionsWith S.union
_typingTable (Ascription expr _ _ _)
	= _typingTable expr
_typingTable (SeqExp exprs _ _)
	= exprs |> _typingTable & M.unionsWith S.union


{- | checks that no two typings do conflict (thus that no two typings result in an empty set for the variables). The typingtable is used for this

>>> import LanguageDef.API
>>> let (Success boolVar) = typeExpression testLanguage' (["TestLanguage"], "bool") (Var "x" () unknownLocation)
>>> boolVar
Var {_varName = "x", _expAnnot = ((),NoIndex {_syntIndForm = (["TestLanguage"],"bool")}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}}
>>> let (Success intVar)  = typeExpression testLanguage' (["TestLanguage"], "int") (Var "x" () unknownLocation)
>>> intVar
Var {_varName = "x", _expAnnot = ((),NoIndex {_syntIndForm = (["TestLanguage"],"int")}), _expLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}}
>>> let supertypings = testLanguage & get langdefs & (M.!["TestLanguage"]) & get (ldScope . payload . langSupertypes)
>>> [boolVar, intVar] ||>> snd & mergeTypings supertypings
Left [("x",fromList [(["TestLanguage"],"bool"),(["TestLanguage"],"int")])]
>>> [boolVar, intVar] ||>> snd & mergeTypings' unknownLocation supertypings & handleFailure toCoParsable show
"| While checking for conflicting variable usage \nError: \n  \8226   x\tis used as TestLanguage.bool, TestLanguage.int"

-}
mergeTypings	:: Lattice FQName -> [Expression SyntFormIndex] -> Either [(String, Set SyntForm)] (Map Name SyntForm)
mergeTypings supertyping exprs
	= do	let typings	= exprs |> _typingTable 
					& M.unionsWith S.union
					|> (id &&& infimums supertyping)
		let failedTypings
				= typings & M.filter ((==) (get bottom supertyping) . snd)
					|> fst
					& M.toList
		unless (null failedTypings) $ Left failedTypings
		return (typings |> snd)

-- Same as mergeTypings, but with an error message
mergeTypings'	:: LocationInfo -> Lattice FQName -> [Expression SyntFormIndex] -> Failable (Map Name SyntForm)
mergeTypings' li st exprs
	= let	showEntry (nm, tps)	= nm++"\tis used as "++ (tps & S.toList |> showFQ & commas)
		in
		inMsg' "While checking for conflicting variable usage" $
			mergeTypings st exprs & first (\msgs -> msgs |> showEntry & unlines & indent)
				& either fail return 


