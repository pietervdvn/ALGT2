module LanguageDef.LangDefsFix where

{- Given a dict of {FQName --> languagedef}, fixes them into scopes, in which a languagedef is contained. This langaugedef will only contain fully qualified stuff, have filled in supertype relationships etc... -}

import Utils.All

import LanguageDef.LanguageDef
import LanguageDef.LocationInfo
import LanguageDef.Syntax.All
import LanguageDef.Syntax.BNF (overRuleCall', getRuleCall)
import LanguageDef.Scope
import LanguageDef.LangDefs
import LanguageDef.FunctionTyper


import Graphs.Lattice (makeLattice, Lattice, debugLattice)

import Control.Arrow ((&&&))
import Data.Bifunctor (first)

import Data.Maybe
import Data.Set as S
import Data.Map as M
import Data.List as L


{- | Input is the raw, parsed information, output is all kind of fixed stuff


>>> import AssetUtils
>>>  let supertypings = testLangDefs & get langdefs & (M.!["TestLanguage"]) & get (ldScope . payload . langSupertypes)
>>> debugLattice showFQ supertypings & putStr
⊤ has following subtypes:  TestLanguage.exprSum
⊥ has following subtypes:<no subs>
TestLanguage.bool has following subtypes:  ⊥
TestLanguage.expr has following subtypes:  TestLanguage.bool
  TestLanguage.int
TestLanguage.exprSum has following subtypes:  TestLanguage.expr
TestLanguage.int has following subtypes:  ⊥

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
asLangDefs		:: Map [Name] (LanguageDef' ResolvedImport ()) -> Either String LangDefs
asLangDefs defs	= do	scopes		<- defs & M.toList |> (fst &&& _scopeFor defs) |+> sndEffect
			scopes'		<- scopes ||>> fixScope |+> sndEffect	:: Either String [([Name], LDScope' ())]
			supertyping	<- createSupertypingRelationship (scopes' ||>> get (ldScope . payload))
			let ld	= scopes' ||>> over (ldScope . payload) (set langSupertypes supertyping)
						& M.fromList 
						& knotScopes 
			ld'	<- typeAllFunctionsLD ld |> LangDefs
			check ld'
			return ld'

bottom	= ([], "⊥")
top	= ([], "⊤")

{- | Creates the lattice tracking the supertype relationship

-}
createSupertypingRelationship	:: [([Name], LanguageDef' ResolvedImport a)] -> Either String (Lattice FQName)
createSupertypingRelationship lds
	= do	let syntaxes	= lds ||>> get langSyntax |> sndEffect & catMaybes ||>> get syntax		:: [([Name], Map Name [(BNF, MetaInfo)])]
		let fqsyntax	= syntaxes ||>> M.toList & unmerge
					|> (\(fq, (nm, cont)) -> ((fq, nm), cont |> fst))
					& M.fromList						:: Map FQName [BNF]
		let supertypes	= fqsyntax ||>> getRuleCall |> catMaybes |> S.fromList		:: Map FQName (Set FQName)
		let subtypes	= invertDict supertypes
		makeLattice bottom top subtypes & first cycleMsg |> fst
		where	cycleMsg	:: [[FQName]] -> String
			cycleMsg cycles
				= "Cycles are detected in the supertype relationship of the syntaxes:"++
					cycles |> (\cycle -> cycle |> showFQ & intercalate " ⊃ ") & unlines & indent
	

_scopeFor	:: Map [Name] (LanguageDef' ResolvedImport fr) -> ([Name], LanguageDef' ResolvedImport fr) -> Either String (LDScope' fr)
_scopeFor ldefs (fqname, ld)
	= do	let imports	= fqname : (ld & get langImports |> get importName)	:: [ [Name] ]
		let units	= repeat ()
		imported	<- imports |+> (\k -> checkExists k ldefs ("Weird, import "++show k++" not found... Bug in LangDefs"))
		let scope	=  Scope
			fqname
			(zip imports units & M.fromList)	-- imported (+ self)
			(zip imports imports & M.fromList)	-- internalView
			ld					-- actual payload/exports; resolve will filter
			M.empty					-- reExports
		return $ LDScope scope ldefs

-- Prepares the language definitions for production (e.g. resolves all calls to be fully qualified). The first argument should be a dict with all the fully fixed scopes
fixScope	:: LDScope' fr -> Either String (LDScope' fr)
fixScope scopeToFix
	= do	let ld	= get (ldScope . payload) scopeToFix
		ld'	<- fixLD scopeToFix ld
		scopeToFix	& set (ldScope . payload) ld'
				& return


fixLD		:: LDScope' fr -> LanguageDef' ResolvedImport fr -> Either String (LanguageDef' ResolvedImport fr)
fixLD scope ld
	= do	let syn		= get langSyntax ld
		syn'		<- syn |> fixSyntax scope & justEffect
		return $ set langSyntax syn' ld

fixSyntax	:: LDScope' fr -> Syntax -> Either String Syntax
fixSyntax scope syn
	= do	let syntx	= get syntax syn
		syntx'		<- syntx & M.toList
					|> (fst &&& fullyQualifySyntForm scope)
					|+> sndEffect |> M.fromList
		return $ set syntax syntx' syn

