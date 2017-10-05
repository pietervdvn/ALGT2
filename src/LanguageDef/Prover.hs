module LanguageDef.Prover where


import Utils.All

import LanguageDef.Utils.ExceptionInfo

import LanguageDef.Data.Rule
import LanguageDef.Data.Proof
import LanguageDef.Data.SyntFormIndex
import LanguageDef.Interpreter


import LanguageDef.LangDefs

-- TODO
proofThat	:: LDScope -> Predicate -> Failable Proof
proofThat lds pred	
	= inContext ("While proving "++toParsable pred, Evaluating, get predLocation pred) $
		constructProof lds pred



constructProof	:: LDScope -> Predicate -> Failable Proof
constructProof lds (PredExpr expr _)
	= do	result	<- evalExpression lds expr
		return $ ProofExpr result
constructProof lds (PredConcl concl _)
	= do	fail $ "TODO "++toParsable concl
