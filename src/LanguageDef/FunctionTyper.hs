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

import Data.Either
import Data.Bifunctor (first)

import AssetUtils	-- TODO Remove

------------------------------- TYPING -------------------------------


type SyntForm	= FQName
data SyntFormIndex = SyntFormIndex
			{ _syntForm	:: FQName
			, _syntChoice	:: Int	-- To what choice does it correspond?
			, _syntSeqInd	:: Maybe Int	-- To what index in the sequence of the choice does it correspond?
			} 
			| NoIndex SyntForm
			deriving (Show)

instance ToString SyntFormIndex where
	toParsable (NoIndex sf)	= showFQ sf
	toParsable (SyntFormIndex f ch ind)
				= showFQ f ++":"++show ch++(ind |> show |> ("."++) & fromMaybe "")

{- | Types an expression to the given expectation (and fully qualifies all calls and types in the mean time)

>>> typeExpression (error "") (["A"], "b") (DontCare ())
Right (DontCare {_expAnnot = ((),NoIndex (["A"],"b"))})
>>> typeExpression testLDScope (["TestLanguage"], "bool") (Var "x" ())
Right (Var {_varName = "x", _expAnnot = ((),NoIndex (["TestLanguage"],"bool"))})
>>> typeExpression testLDScope (["TestLanguage"], "bool") (ParseTree (simplePT "True") ())
Right (ParseTree {_expPT = Literal {_ptToken = "True", ...}, _expAnnot = ((),SyntFormIndex {_syntForm = (["TestLanguage"],"bool"), _syntChoice = 0, _syntSeqInd = Just 0})})

 -}
typeExpression	:: LDScope -> SyntForm -> Expression a -> Either String (Expression (a, SyntFormIndex)) 
typeExpression _ expectation (Var nm a)
	= return $ Var nm (a, NoIndex expectation)
typeExpression _ expectation  (DontCare a)
	= return $ DontCare (a, NoIndex expectation)
typeExpression ld expectation (FuncCall funcNm args a)
	= do	(fqname, function)	<- resolve' ld functionCall funcNm
		argTypes		<- get funcArgTypes function |+> resolve ld syntaxCall
		args'			<- zip argTypes args |+> uncurry (typeExpression ld)
		let ld'			= get payload ld
		ftype	<- get funcRetType function & resolve ld syntaxCall
		assert (isSubtypeOf ld' ftype expectation || isSubtypeOf ld' expectation ftype)
			$ "No common ground"
		return (FuncCall fqname args' (a, NoIndex ftype))
typeExpression ld expectation (Ascription expr name a)
	= do	name'	<- resolve ld syntaxCall name
		expr'	<- typeExpression ld name' expr
		assert (isSubtypeOf (get payload ld) name' expectation)
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
typeExpressionIndexed	:: LDScope -> SyntForm -> [Expression a] -> Either String ([Expression (a, SyntFormIndex)], SyntFormIndex)
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


typeExpressionIndexed'	:: LDScope -> SyntForm -> [Expression a] -> (Int, [BNF]) -> Either String [Expression (a, SyntFormIndex)]
typeExpressionIndexed' ld syntForm exprs (choiceIndex, choiceElems)
 | length exprs /= length choiceElems
	= Left $ "Can not match choice "++show choiceIndex++": this choice has "++show (length choiceElems) ++ 
			" elements, whereas the expression has "++show (length exprs)
 | otherwise
	= do	result	<- zip (mapi choiceElems) exprs |+> uncurry (typeExprBNF ld (syntForm, choiceIndex))
		return result


typeExprBNF		:: LDScope -> (SyntForm, Int) -> (Int, BNF) -> Expression a -> Either String (Expression (a, SyntFormIndex))
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

-- TODO add to tests: typeExpression testLDScope (["TestLanguage"], "bool") (ParseTree (simplePT "True") ())

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

