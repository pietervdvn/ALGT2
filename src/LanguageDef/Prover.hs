module LanguageDef.Prover where


import Utils.All

import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.Grouper

import LanguageDef.Data.ParseTree
import LanguageDef.Data.Relation
import LanguageDef.Data.Rule
import LanguageDef.Data.Proof
import LanguageDef.Data.SyntFormIndex
import LanguageDef.Data.LanguageDef

import LanguageDef.Interpreter
import LanguageDef.Typer (modedArgs, selectModed)


import LanguageDef.LangDefs

import qualified Data.Map as M
import Data.Map (Map)
import Data.List (nub)

import Control.Arrow ((&&&))
import Control.Monad

proofThat	:: LDScope -> Predicate -> Failable Proof
proofThat lds pred	
	= inContext ("While proving "++toParsable pred, Evaluating, get predLocation pred) $
		(constructProof lds M.empty pred |> fst)



constructProof	:: LDScope -> VariableStore -> Predicate -> Failable (Proof, VariableStore)
constructProof lds vars (PredExpr expr _)
	= do	result	<- constructParseTree lds vars expr
		return (ProofExpr result, vars)
constructProof lds vars (PredConcl concl _)
	= do	(fqn, rel)	<- resolve' lds relationCall (get conclRelName concl)
		lds'		<- lds & enterScope (fst fqn)
		foundRules	<- rulesAbout lds' fqn	
		inExprs		<- modedArgs lds' In concl
		inArgs		<- inExprs |> constructParseTree lds vars & allGood
		let individual	= foundRules |> proofRule lds inArgs
		let successfull	= successess individual
		when (null successfull) $ inMsg' "No rule matched" $
			fails individual & Aggregate & Failed
		
		let concls@(proof:rest)
				= successfull |> get proofConcl
		
		assert' (all ((==) proof) rest) $
			"The proof is divergent; the rules can be interpreted in multiple ways yielding multiple results, namely: \n" ++
			(concls & nub |> toParsable' ", " & intercalate "\n" & indent)
		
		outExprs	<- modedArgs lds' Out concl
		outArgs		<- proof & selectModed lds' concl Out	:: Failable [ParseTree]
		vars'		<- inMsg' ("While calculating the final variable store") $ patternMatchAll lds' vars outExprs outArgs
		return (head successfull, vars')




proofRule	:: LDScope -> [ParseTree] -> Rule -> Failable Proof
proofRule lds inArgs rule@(Rule preds concl name docs)
	= inMsg' ("While trying to proof "++show name) $ inLocation (get miLoc docs) $
	  do	inExprs			<- modedArgs lds In concl
		initialVarStore		<- patternMatchAll lds M.empty inExprs inArgs
		(endVars, predProofs)	<- foldM (\(vars, proofs) pred -> 
						do	(proof, vars')	<- constructProof lds vars pred
							return (vars', proof:proofs)) (initialVarStore, []) preds
		outExprs		<- modedArgs lds Out concl
		outArgs			<- outExprs |> constructParseTree lds endVars & allGood
		rel			<- resolve_ lds relationCall (get conclRelName concl)
		let modes		= rel & get relTypes |> snd
		let args		= interleaveArgs modes inArgs outArgs
		let proof	= Proof args rule predProofs
		return proof


interleaveArgs	:: [Mode] -> [a] -> [a] -> [a]
interleaveArgs (In:modes) (i:ins) outs
		= i:interleaveArgs modes ins outs
interleaveArgs (Out:modes) ins (out:outs)
		= out:interleaveArgs modes ins outs
interleaveArgs [] [] []
		= []
interleaveArgs _ _ _
		= error $ "Number of arguments does not match"


rulesAbout	:: LDScope -> FQName -> Failable [Rule]
rulesAbout lds relFQN
	= do	rules	<- lds & get (ldScope . langRules)	
				& maybe (fail "No relations are implemented") return
		let allRules	= get grouperDict rules & M.elems
					|> (get ruleAbout &&& id)
					& merge
					& M.fromList	:: Map FQName [Rule]
		checkExistsSuggDist' distFQ relFQN allRules ("No rules found implementing "++showFQ relFQN)
		
