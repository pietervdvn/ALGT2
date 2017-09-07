module LanguageDef.Prover where


import Utils.Utils

import LanguageDef.Syntax
import LanguageDef.Rule
import LanguageDef.Relation
import LanguageDef.Proof
import LanguageDef.LangDefs

import LanguageDef.API
import AssetUtils 


proofThat	:: LangDefs -> Relation -> [ParseTree a] -> Proof a
proofThat rel args
	= todo



proofRule	:: LangDefs -> Rule -> [ParseTree a] -> Either String (Proof a)
proofRule lds (Rule [] concl name docs) args
	= do	let relation	= get conclRelName concl
		Left $ show relation




t	= do	let fqn x	= (["TestLanguage"], x)
		let tld		= testLangDefs
		rule		<- resolveGlobal' testLangDefs ruleCall (fqn "and")
		trAndTr		<- createParseTree tld (fqn "exprSum") "PRover.hs" "True & True"
		let args	= [trAndTr]
		proofRule tld rule args

