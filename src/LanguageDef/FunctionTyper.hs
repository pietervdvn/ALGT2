module LanguageDef.FunctionTyper where

{- Types expressions and functions -}

import Utils.All

import LanguageDef.LanguageDef
import LanguageDef.LocationInfo
import LanguageDef.MetaExpression
import LanguageDef.MetaFunction
import LanguageDef.LangDefs
import LanguageDef.Scope
import LanguageDef.Syntax.BNF (BNF)
import qualified LanguageDef.Syntax.BNF as BNF
import LanguageDef.Syntax

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

------------------------------- TYPING -------------------------------


typeAllFunctionsLD	:: Map [Name] (LDScope' ()) -> Either String (Map [Name] LDScope)
typeAllFunctionsLD lds
	= do	typed	<- lds & M.toList ||>> typeScope |> sndEffect & allRight' |> M.fromList
				:: Either String (Map [Name] (Scope [Name] [Name] LanguageDef () ()))
		let env	= typed |> get payload
		let scopes	= typed |> flip LDScope env	:: Map [Name] LDScope
		return scopes


typeScope	:: LDScope' () -> Either String (Scope [Name] [Name] LanguageDef () ())
typeScope scope
	= do	let langDef	= get (ldScope . payload) scope
		langDef'	<- typeAllFunctions scope langDef
		let scope'	= scope & get ldScope & set payload langDef'
		return scope'


typeAllFunctions	:: LDScope' fr -> LanguageDef' ResolvedImport () -> Either String LanguageDef
typeAllFunctions ld langDef
 | isNothing (get langFunctions langDef)
	= langDef & set langFunctions Nothing & return
 | otherwise
	= do	let supers	= get langSupertypes langDef
		let funcs	= get langFunctions langDef & fromJust
		let bareFunctions
				= funcs & get functions
					& M.toList	:: [(Name, Function ())]
		bareFunctions'	<- bareFunctions ||>> typeFunction ld supers |> sndEffect & allRight'
		let funcs'	= funcs & set functions  (bareFunctions' & M.fromList)
		let langDef'	= set langFunctions (Just funcs') langDef
		return langDef'
		


typeFunction	:: LDScope' fr -> Lattice FQName -> Function () -> Either String (Function SyntFormIndex)
typeFunction ld supers f
	= inMsg ("While typing "++ get funcName f) $
	  do	let clauses	= get funcClauses f
		let tps		= (get funcArgTypes f, get funcRetType f)
		clauses'	<- clauses & mapi |> typeClause ld supers tps & allRight'
		f & set funcClauses clauses' & return
		


typeClause	:: LDScope' fr -> Lattice FQName -> ([FQName], FQName) -> (Int, FunctionClause ()) -> Either String (FunctionClause SyntFormIndex)
typeClause scope supertypings (patExps, retExp) (i, FunctionClause pats ret doc nm)
 | length patExps /= length pats
	= Left $ "Expected "++show (length patExps)++" patterns for function "++show nm++", but got "++show (length pats)++" patterns instead"
 | otherwise
	= inMsg ("While typing clause "++nm++"."++show i) $ do
		pats'	<- zip patExps pats & mapi |> (\(i, (patExp, pat)) -> typePattern ("pattern "++show i) scope supertypings patExp pat)
				& allRight'
		ret'	<- typePattern "return expression" scope supertypings retExp ret	-- typing of the main return expression
		inMsg "While checking for conflicting variable usage between patterns" $ mergeTypings' supertypings pats'
		inMsg "While checking for conflicting variable usage between patterns and the clause body" $ mergeTypings' supertypings (ret':pats')
		return $ FunctionClause pats' ret' doc nm



typePattern	:: String -> LDScope' fr -> Lattice FQName -> FQName -> Expression ()  -> Either String (Expression SyntFormIndex)
typePattern msg ld supers exp pat
	= inMsg ("While typing "++msg++", namely "++toParsable pat) $ do
		pat'	<- typeExpression ld exp pat ||>> snd
		mergeTypings' supers [pat']
		return pat'

{- | Types an expression to the given expectation (and fully qualifies all calls and types in the mean time)

>>> import AssetUtils

>>> typeExpression (error "") (["A"], "b") (DontCare ())
Right (DontCare {_expAnnot = ((),NoIndex (["A"],"b"))})
>>> typeExpression testLDScope (["TestLanguage"], "bool") (Var "x" ())
Right (Var {_varName = "x", _expAnnot = ((),NoIndex (["TestLanguage"],"bool"))})
>>> typeExpression testLDScope (["TestLanguage"], "bool") (ParseTree (simplePT "True") ())
Right (ParseTree {_expPT = Literal {_ptToken = "True", ...}, _expAnnot = ((),SyntFormIndex {_syntForm = (["TestLanguage"],"bool"), _syntChoice = 0, _syntSeqInd = Just 0})})
 -}
typeExpression	:: LDScope' fr -> SyntForm -> Expression a -> Either String (Expression (a, SyntFormIndex)) 
typeExpression _ expectation (Var nm a)
	= return $ Var nm (a, NoIndex expectation)
typeExpression _ expectation  (DontCare a)
	= return $ DontCare (a, NoIndex expectation)
typeExpression ld expectation (FuncCall funcNm args a)
	= do	(fqname, function)	<- resolve' ld functionCall funcNm
		argTypes		<- get funcArgTypes function |+> resolve ld syntaxCall
		args'			<- zip argTypes args |+> uncurry (typeExpression ld)
		let ld'			= get (ldScope . payload) ld
		ftype	<- get funcRetType function & resolve ld syntaxCall
		assert (isSubtypeOf ld' ftype expectation || isSubtypeOf ld' expectation ftype)
			"No common ground"
		return (FuncCall fqname args' (a, NoIndex ftype))
typeExpression ld expectation (Ascription expr name a)
	= do	name'	<- resolve ld syntaxCall name
		expr'	<- typeExpression ld name' expr
		assert (isSubtypeOf (get (ldScope . payload) ld) name' expectation)
			$ "The ascription of "++toParsable expr++" as a "++show name++" is not a subtype of "++showFQ expectation
		return $ Ascription expr' name' (a, NoIndex name')
typeExpression ld expectation pt@ParseTree{}
	= do	(expr', _)	<- typeExpressionIndexed ld expectation [pt]
		assert (length expr' == 1) "Huh, this is weird. Bug in typeExpression"
		return $ head expr'
typeExpression ld expectation (SeqExp exprs a)
	= do	(exprs', subtype)	<- typeExpressionIndexed ld expectation exprs
		return $ SeqExp exprs' (a, subtype)

{-Search for a choice matching the expressions -}
typeExpressionIndexed	:: LDScope' fr -> SyntForm -> [Expression a] -> Either String ([Expression (a, SyntFormIndex)], SyntFormIndex)
typeExpressionIndexed ld syntForm exprs
	= do	(fqname, choices)	<- resolve' ld syntaxCall syntForm
		let tries	= choices |> BNF.removeWS |> BNF.unsequence	-- prepare individual choices
					& mapi 					-- Number the choices
					|> typeExpressionIndexed' ld syntForm exprs	-- Actually type them
					& mapi |> sndEffect				-- and number it again
		let successfull	= rights tries		--	:: [(Int, [Expression (a, SyntFormIndex)])]
		assert (not $ null successfull) $ "Could not type the sequence "++ unwords (exprs |> toParsable)++" as a "++showFQ syntForm++":\n"
			++ lefts tries & unlines & indent
		assert (length successfull < 2) $ "The sequence "++ unwords (exprs |> toParsable)++" could be typed in multiple ways, namely as:\n"
			++ (successfull |> (\(i, exprs) -> "Via choice "++show i) & unlines & indent)
		let (foundInd, foundExprs)	= head successfull
		(foundExprs, SyntFormIndex fqname foundInd Nothing) & return


typeExpressionIndexed'	:: LDScope' fr -> SyntForm -> [Expression a] -> (Int, [BNF]) -> Either String [Expression (a, SyntFormIndex)]
typeExpressionIndexed' ld syntForm exprs (choiceIndex, choiceElems)
 | length exprs /= length choiceElems
	= Left $ "Can not match choice "++show choiceIndex++": this choice has "++show (length choiceElems) ++ 
			" elements, whereas the expression has "++show (length exprs)
 | otherwise
	= zip (mapi choiceElems) exprs |+> uncurry (typeExprBNF ld (syntForm, choiceIndex))


typeExprBNF		:: LDScope' fr -> (SyntForm, Int) -> (Int, BNF) -> Expression a -> Either String (Expression (a, SyntFormIndex))
typeExprBNF ldscope (form, choiceInd) (seqIndex, BNF.RuleCall fqname) expr
	= typeExpression ldscope fqname expr
typeExprBNF ldscope (form, choiceInd) (seqIndex, bnf) expr
	-- At this point, the bnf can not be: RuleCall (prev. case), Seq (BNF.unsequence was run in typeExpressionIndexed)
	-- Still resting: Literal, Builtin, Group -- which all should have a matching parsetree (or dontcare)
	= let	fi	= SyntFormIndex form choiceInd (Just seqIndex) in 
	  inMsg ("While typing the expression "++ toParsable expr++ " against "++toParsable bnf ++ inParens (toParsable fi)) $
	  case expr of
		(ParseTree pt a)	-> _compareBNFPT bnf pt >> return (ParseTree pt (a, fi))
		(DontCare a)		-> return (DontCare (a, fi))
		(Var nm a)		-> return (Var nm (a, fi))
		a@Ascription{}		-> Left $ "Found ascription "++toParsable a++" where a literal value of the form "++toParsable bnf++
							" was expected; ascriptions can only match rulecalls"
		s@SeqExp{}		-> Left $ "Found a sequence "++toParsable s++" where a literal value of the form "++toParsable bnf++
							" was expected; ascriptions can only match rulecalls"

_compareBNFPT	:: BNF -> ParseTree a -> Either String ()
_compareBNFPT (BNF.Literal str) (Literal token  _ _ _)
	= assert (str == token) $ "Expected a literal "++show str++" but got the token "++show token
_compareBNFPT (BNF.Literal str) (Int token  _ _)
	= assert (str == show token) $ "Expected a literal "++show str++" but got the int "++show token
_compareBNFPT (BNF.BuiltIn _ builtin) (Literal token _ _ _)
	= assert (token `BNF.isElementOf` builtin) $ "Expected a "++get BNF.biName builtin++", but got "++show token
_compareBNFPT (BNF.BuiltIn _ builtin) (Int token _ _)
 	= assert (builtin `elem` [BNF.intBI, BNF.numberBI]) $ "Expected a "++get BNF.biName builtin++", but got the int"++show token
_compareBNFPT (BNF.RuleCall name) re@RuleEnter{}
	= let	actRule = get ptUsedRule re in
		assert (name == actRule) $ "Expected the rule "++showFQ name++", but got a parsetree constructed with "++showFQ actRule++": "++toParsable re
_compareBNFPT (BNF.Group bnf) pt
	= _compareBNFPT bnf pt
_compareBNFPT (BNF.Seq bnf) pt
	= error "Bug: sequence should not be reachable here"



{- | Maps an expression onto all possible types it assumes
>>> _typingTable $ DontCare {_expAnnot = (NoIndex (["A"],"b"))}
fromList []
>>> let varX = Var {_varName = "x", _expAnnot = (NoIndex (["TestLanguage"],"bool"))}
>>> _typingTable varX
fromList [("x",fromList [(["TestLanguage"],"bool")])]
>>> let varInt = Var {_varName = "x", _expAnnot = (NoIndex (["TestLanguage"],"int"))}
>>> let seq = SeqExp [varX, varInt] (error "No index here")
>>> _typingTable seq
fromList [("x",fromList [(["TestLanguage"],"bool"),(["TestLanguage"],"int")])]

-}
_typingTable	:: Expression SyntFormIndex -> Map Name (Set FQName)
_typingTable (Var nm (NoIndex sf))
	= M.singleton nm (S.singleton sf)
_typingTable (DontCare _)
	= M.empty
_typingTable (FuncCall _ args _)
	= args |> _typingTable & M.unionsWith S.union
_typingTable (Ascription expr _ _)
	= _typingTable expr
_typingTable (SeqExp exprs _)
	= exprs |> _typingTable & M.unionsWith S.union


{- | checks that no two typings do conflict (thus that no two typings result in an empty set for the variables). The typingtable is used for this

>>> import AssetUtils
>>> let (Right boolVar) = typeExpression testLDScope (["TestLanguage"], "bool") (Var "x" ())
>>> boolVar
Var {_varName = "x", _expAnnot = ((),NoIndex (["TestLanguage"],"bool"))}
>>> let (Right intVar)  = typeExpression testLDScope (["TestLanguage"], "int") (Var "x" ())
>>> intVar
Var {_varName = "x", _expAnnot = ((),NoIndex (["TestLanguage"],"int"))}

>>> let supertypings = testLangDefs & get langdefs & (M.!["TestLanguage"]) & get (ldScope . payload . langSupertypes)
>>> [boolVar, intVar] ||>> snd & mergeTypings supertypings
Left [("x",fromList [(["TestLanguage"],"bool"),(["TestLanguage"],"int")])]
>>> [boolVar, intVar] ||>> snd & mergeTypings' supertypings
Left "While checking for conflicting variable usage:\n    x\tis used as TestLanguage.bool, TestLanguage.int"
-}
mergeTypings	:: Lattice FQName -> [Expression SyntFormIndex] -> Either [(String, Set SyntForm)] ()
mergeTypings supertyping exprs
	= do	let typings	= exprs |> _typingTable 
					& M.unionsWith S.union
					& M.filter (not . S.null)
					& M.filter ((/=) 1 . S.size)
					|> (id &&& infimums supertyping)
		let failedTypings
				= typings & M.filter ((==) (get bottom supertyping) . snd)
					|> fst
					& M.toList
		unless (null failedTypings) $ Left failedTypings

-- Same as mergeTypings, but with an error message
mergeTypings' st exprs
	= let	showEntry (nm, tps)	= nm++"\tis used as "++ (tps & S.toList |> showFQ & commas)
		in
		inMsg "While checking for conflicting variable usage" $
			mergeTypings st exprs & first (\msgs -> msgs |> showEntry & unlines & indent)


