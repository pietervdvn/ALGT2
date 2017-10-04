{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module LanguageDef.Proof where

{- Creates a proof for a bunch of rules -}

import Utils.All

import LanguageDef.Relation
import LanguageDef.Rule
import LanguageDef.Expression
import LanguageDef.Tools.LocationInfo
import LanguageDef.Syntax
import LanguageDef.Tools.ExceptionInfo


{-
When a rule is applied to enough values (parsetrees) it generates a proof of this rule.
-}
data Proof a	= Proof { _proofConcl	:: [ParseTree ()]	-- output of the conclusion of the rule
			, _proofWith	:: Rule	-- rule which has proven stuff
			, _proofPreds	:: [Proof a]	-- predicates for the rule
			}
		 deriving (Show, Eq)

makeLenses ''Proof


{-Number of 'layers' in the proof-}
depth	:: Proof a -> Int
depth proof
 | null (get proofPreds proof)
		=  1
 | otherwise	=  get proofPreds proof |> depth & maximum & (+1)


{-Number of proof elements-}
weight	:: Proof a -> Int
weight proof
	 = 1 + (proof & get proofPreds |> weight & sum)




data ProofOptions	= PO {	nameParens		:: String -> String,
				showNames		:: Bool,
				betweenPredicates	:: String }


defaultProofOptions	= PO (\s -> "["++s++"]") True "    "


-- Extra options to print proofs
data ProofOptions' a	= PO' {	opts'		:: ProofOptions,
				sp		:: ParseTree' -> String,
				se		:: Expression a -> String,
				sr		:: Rule -> String
				}

instance ToString (Proof a) where
	toParsable	= toParsable' defaultProofOptions
	toCoParsable	= toCoParsable' defaultProofOptions
	debug		= debug' defaultProofOptions


instance ToString' ProofOptions (Proof a) where
	show' po proof		= let opts	= PO' po show debug show 			in showProofWith opts proof & unlines
	toParsable' po proof	= let opts	= PO' po toParsable toParsable toCoParsable 	in showProofWith opts proof & unlines
	toCoParsable' po proof	= let opts	= PO' po toCoParsable toParsable toParsable 	in showProofWith opts proof & unlines
	debug' po proof		= let opts	= PO' po debug debug debug			in showProofWith opts proof & unlines
		

-- shows a proof part; returns lines 
showProofWith	:: ProofOptions' a -> Proof a -> [String]
showProofWith opts (Proof concl proverRule predicates)
	= let	options	= opts' opts
		preds'	= predicates |> showProofWith opts
		preds''	= if null preds' then [] else init preds' ||>> (++ betweenPredicates options)  ++ [last preds']
		preds	= preds'' & foldl (stitch ' ') []	:: [String]
		predsW	= ("":preds) |> length & maximum	:: Int
		conclArgs	= concl |> sp opts & commas
		concl'	= (proverRule & get (ruleConcl . conclRelName) & showFQ & inParens) ++ conclArgs
		lineL	= max predsW (length concl')
		name	= if showNames options then " " ++ nameParens options (get ruleName proverRule) else ""
		lineL'	= lineL - if 3 * length name <= lineL && predsW == lineL then length name else 0
		line	= replicate lineL' '-'		:: String
		line'	= line ++ name
		in
		(preds ++ [line', concl'])



showProofWithDepth		:: String -> Name -> ProofOptions-> Proof a -> String
showProofWithDepth input relation options proof
	= ["# "++input++" applied to "++relation
		,"# Proof weight: "++show (weight proof)++", proof depth: "++ show (depth proof) 
		, ""
		, ""
		, toParsable' options proof, "", "", ""] & unlines
