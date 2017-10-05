 {-# LANGUAGE RankNTypes #-}
module LanguageDef.LangDefsFix where

{- Given a dict of {FQName --> languagedef}, fixes them into scopes, in which a languagedef is contained. This langaugedef will only contain fully qualified stuff, have filled in supertype relationships etc... -}

import Utils.All

import LanguageDef.LanguageDef
import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.Checkable

import LanguageDef.Utils.Grouper
import LanguageDef.Syntax.All
import LanguageDef.Syntax.BNF (overRuleCall', getRuleCall)
import LanguageDef.LangDefs
import LanguageDef.Typer
import LanguageDef.Function

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
>>> let supertypings = testLanguage' & get (ldScope . langSupertypes)
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
asLangDefs		:: Map [Name] (LanguageDef' ResolvedImport ()) -> Failable LangDefs
asLangDefs defs	= do	scopes		<- defs & M.toList |> (fst &&& _scopeFor defs) |> sndEffect & allGood
			scopes'		<- scopes ||>> fixScope |+> sndEffect	:: Failable [([Name], LDScope' ())]
			supertyping	<- createSupertypingRelationship (scopes' ||>> get ldScope)
			let ld	= scopes' ||>> over ldScope (set langSupertypes supertyping)
						& M.fromList 
						& knotScopes 
			ld'	<- typeLD ld |> LangDefs
			inPhase Validating $ check ld'
			return ld'


{- | Creates the lattice tracking the supertype relationship

-}
createSupertypingRelationship	:: [([Name], LanguageDef' ResolvedImport a)] -> Failable (Lattice FQName)
createSupertypingRelationship lds
	= do	let syntaxes	= lds ||>> get langSyntax |> sndEffect & catMaybes 
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
			 & either fail return & inMsg' "While constructing the global supertyping relationship" & inPhase Typing
	

_scopeFor	:: Map [Name] (LanguageDef' ResolvedImport fr) -> ([Name], LanguageDef' ResolvedImport fr) -> Failable (LDScope' fr)
_scopeFor ldefs (fqname, ld)
	= do	let file	= ld & get (langLocation . miFile)
		let selfImport	= (fqname, ImportFlags file True (tails fqname))
		let imports	= ld & get langImports |> (get importName &&& _importFlagFor)	:: [ ([Name], ImportFlags) ]
		imported	<- imports |+> (\k -> checkExists' (fst k) ldefs ("Weird, import "++show k++" not found... Bug in LangDefs"))
		return $ LDScope ld (M.fromList (selfImport:imports)) ldefs

_importFlagFor	:: Import ResolvedImport -> ImportFlags
_importFlagFor (Import name qualifiedOnly _ filePath)	-- name is the absolute path from the root directory here
	= ImportFlags filePath False (if qualifiedOnly then [name] else tails name)

-- Prepares the language definitions for production (e.g. resolves all calls to be fully qualified). The first argument should be a dict with all the fully fixed scopes
fixScope	:: LDScope' fr -> Failable (LDScope' fr)
fixScope scopeToFix
	= do	let ld	= get ldScope scopeToFix
		ld'	<- fixLD scopeToFix ld
		scopeToFix	& set ldScope ld'
				& return


fixLD		:: LDScope' fr -> LanguageDef' ResolvedImport fr -> Failable (LanguageDef' ResolvedImport fr)
fixLD scope ld
	= ld	& overGrouperLens langSyntax (fullyQualifySyntForm scope)
		>>= overGrouperLens langFunctions (fullyQualifyFunction scope)
		>>= overGrouperLens langRelations (fullyQualifyRelation scope)
		>>= overGrouperLens langRules (fullyQualifyRule scope)





