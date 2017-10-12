 {-# LANGUAGE RankNTypes #-}
module LanguageDef.LangDefsFix where

{- Given a dict of {FQName --> languagedef}, fixes them into scopes, in which a languagedef is contained. This langaugedef will only contain fully qualified stuff, have filled in supertype relationships etc... -}

import Utils.All

import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.Checkable
import LanguageDef.Utils.Grouper

import LanguageDef.Data.BNF (BNF, overRuleCall', getRuleCall)
import LanguageDef.Data.SyntacticForm
import LanguageDef.Data.LanguageDef
import LanguageDef.LangDefs
import LanguageDef.Typer
import LanguageDef.Data.Function
import LanguageDef.MetaSyntax (typeTop, typeBottom)


import Graphs.Lattice (makeLatticeInverted, Lattice, debugLattice)

import Control.Arrow ((&&&))
import Data.Bifunctor (first)

import Data.Maybe
import Data.Set as S
import Data.Map as M
import Data.List as L

import Lens.Micro (Lens)


{- | Input is the raw, parsed information, output is all kind of fixed stuff


>>> import AssetUtils
>>> import LanguageDef.API
>>> let supertypings = testLanguage & get (ldScope . langSupertypes)
>>> debugLattice showFQ supertypings & putStr
⊥ has following subtypes:<no subs>
TestLanguage.bool has following subtypes:  ⊥
TestLanguage.expr has following subtypes:  TestLanguage.bool
  TestLanguage.int
TestLanguage.exprSum has following subtypes:  TestLanguage.expr
TestLanguage.int has following subtypes:  ⊥
TestLanguage.op has following subtypes:  ⊥
TestLanguage.tuple has following subtypes:  ⊥

>>> import Graphs.Lattice
>>> infimums supertypings [(["TestLanguage"], "bool")]
(["TestLanguage"],"bool")
>>> infimums supertypings [(["TestLanguage"], "bool"), (["TestLanguage"], "bool")]
(["TestLanguage"],"bool")
>>> infimums supertypings [(["TestLanguage"], "bool"), (["TestLanguage"], "expr")]
(["TestLanguage"],"bool")
>>> infimums supertypings [(["TestLanguage"], "bool"), (["TestLanguage"], "int")]
([],"\8869")
 -}
asLangDefs		:: Map [Name] (LanguageDef' ResolvedImport ()) -> Failable (Map [Name] LDScope)
asLangDefs defs	= do	scopes		<- defs & M.toList |> (fst &&& _scopeFor defs) |> sndEffect & allGood	:: Failable [([Name], LDScope' ())]
			scopes'		<- scopes & M.fromList & knotScopes & M.toList 
						||>> fullyQualifyScope |> sndEffect & allGood
			supertyping	<- createSupertypingRelationship (scopes' ||>> get ldScope)
		
			let ldDict	= scopes' 
						||>> over ldScope (set langSupertypes supertyping)
						& M.fromList 
						& knotScopes 
			ld'	<- typeLD ldDict
			inPhase Validating (ld' & M.toList |> uncurry check' & allGood )
			return ld'


knotScopes	:: Map [Name] (LDScope' fr) -> Map [Name] (LDScope' fr)
knotScopes ldefs
	= let	ldefs'	= ldefs |> set environment ldefs'
		in
		ldefs'



_scopeFor	:: Map [Name] (LanguageDef' ResolvedImport fr) -> ([Name], LanguageDef' ResolvedImport fr) -> Failable (LDScope' fr)
_scopeFor ldefs (fqname, ld)
	= do	let file	= ld & get (langLocation . miFile)
		let selfImport	= (fqname, ImportFlags file True (tails fqname))
		let imports	= ld & get langImports |> (get importName &&& _importFlagFor)	:: [ ([Name], ImportFlags) ]
		imported	<- imports |+> (\k -> checkExists' (fst k) ldefs ("Weird, import "++show k++" not found... Bug in LangDefs"))
		return $ LDScope ld (M.fromList (selfImport:imports)) (error "Bug: No environment given yet; knot the scopes first")


_importFlagFor	:: Import ResolvedImport -> ImportFlags
_importFlagFor (Import name qualifiedOnly _ filePath)	-- name is the absolute path from the root directory here
	= ImportFlags filePath False (if qualifiedOnly then [name] else tails name)




-- Prepares the language definitions for production (e.g. resolves all calls to be fully qualified). The first argument should be a dict with all the fully fixed scopes
fullyQualifyScope	:: LDScope' fr -> Failable (LDScope' fr)
fullyQualifyScope scopeToFix
	= do	let ld	= get ldScope scopeToFix
		ld'	<- fullyQualifyLangDef scopeToFix ld
		return $ set ldScope ld' scopeToFix


fullyQualifyLangDef		:: LDScope' fr -> LanguageDef' ResolvedImport fr -> Failable (LanguageDef' ResolvedImport fr)
fullyQualifyLangDef scope ld
	= ld	& overGrouperLens langSyntax (fullyQualifySyntForm scope)
		>>= overGrouperLens langFunctions (fullyQualifyFunction scope)
		>>= overGrouperLens langRelations (fullyQualifyRelation scope)
		>>= overGrouperLens langRules (fullyQualifyRule scope)


{- | Creates the lattice tracking the supertype relationship

-}
createSupertypingRelationship	:: [([Name], LanguageDef' ResolvedImport a)] -> Failable (Lattice FQName)
createSupertypingRelationship lds
	= inMsg' "While constructing the global supertyping relationship" $ inPhase Typing $
	  do	let syntaxes	= lds ||>> get langSyntax |> sndEffect & catMaybes 
					||>> get grouperDict
					|||>>> get syntChoices 		:: [([Name], Map Name [BNF])]
		let fqsyntax	= syntaxes
					|> (\(fq, syntForms) -> syntForms & mapKeys ((,) fq) )
					& M.unions
										:: Map FQName [BNF]
		let supertypes	= fqsyntax ||>> getRuleCall |> catMaybes |> S.fromList		:: Map FQName (Set FQName)
		let cycleMsg cycles
				= "Cycles are detected in the supertype relationship of the syntaxes:"++
					cycles |> (\cycle -> cycle |> showFQ & intercalate " ⊃ ") & unlines & indent
		makeLatticeInverted typeBottom typeTop supertypes & first cycleMsg |> fst
			 & either fail return
	



