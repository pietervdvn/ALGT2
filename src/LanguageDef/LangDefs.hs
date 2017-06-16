module LanguageDef.LangDefs where

{-

Definition of a cluster language definitions.

See also: LanguageDef.LangDefsFix

-}

import Utils.All


import LanguageDef.LanguageDef
import LanguageDef.LocationInfo
import LanguageDef.Syntax.All
import LanguageDef.Syntax.BNF (overRuleCall', getRuleCall)
import LanguageDef.Scope
import LanguageDef.MetaFunction

import Graphs.Lattice (makeLattice, Lattice, debugLattice)

import Control.Arrow ((&&&))
import Data.Bifunctor (first)

import Data.Maybe
import Data.Set as S
import Data.Map as M
import Data.List as L


type LDScope	= Scope [Name] [Name] LanguageDef ({-Import flags-}) ({-Re-export flags-})

{- Contains the full cluster of language defintions + checks -}
data LangDefs	= LangDefs {_langdefs	:: Map [Name] LDScope}
	deriving (Show)


fullyQualifySyntForm
			:: LDScope -> (Name, [(BNF, MetaInfo)]) -> Either String [(BNF, MetaInfo)]
fullyQualifySyntForm scope (syntFormName, choices)
	= inMsg ("In the definition of "++syntFormName) $
		choices |> (fullyQualifyBNF scope &&& snd) |+> fstEffect


fullyQualifyBNF	:: LDScope -> (BNF, MetaInfo) -> Either String BNF
fullyQualifyBNF scope (bnf, metaInfo)
	= inMsg ("In a choice "++toCoParsable (get miLoc metaInfo)) $
		bnf & overRuleCall' (resolve scope syntaxCall)



syntaxCall	:: (String, LanguageDef -> Maybe Syntax, Syntax -> Map Name [BNF])
syntaxCall	= ("the syntactic form", get langSyntax, \synt -> get syntax synt ||>> fst)

functionCall	:: (String, LanguageDef -> Maybe Functions, Functions -> Map Name TypedFunction)
functionCall	= ("the syntactic form", get langFunctions, get functions)

resolve	:: LDScope -> (String, LanguageDef -> Maybe a, a -> Map Name b) -> FQName -> Either String FQName
resolve	scope entity name
	= resolve' scope entity name |> fst

resolve'	:: LDScope ->  (String, LanguageDef -> Maybe a, a -> Map Name b) -> FQName -> Either String (FQName, b)
resolve' scope (entity, getWhole, getPart) ([], name)
	-- the localscope/direct import case
	= do	let getDict ld	= ld & getWhole & maybe M.empty getPart
		let getKnown ld	= getDict ld & keys	:: [Name]
		let ld		= scope & get payload
		if name `elem` (getKnown ld) then
			-- we found it locally
			return ((get scopeName scope, name), getDict ld ! name)
		else do
			-- Lets take a look at the imports
			let foundNSes	= 
				implicitImports scope |> getKnown	-- { [Name] (path) --> [Name] (known syntactic forms, of which we search one }
					& M.filter (name `elem`)	-- Name is an element of the knownNames
			let validNSes	= foundNSes & M.keys		-- These namespaces export an element called `name`
			assert (length validNSes < 2) $
				["Multiple namespaces export "++entity++" "++show name++", namely: "
				, validNSes |> intercalate "." & unlines & indent
				, "Add a prefix to disambiguate the exact object you want"
				] & unlines
			assert (not $ L.null validNSes) $ uppercase entity ++" "++ show name ++ " was not found. It is not defined in the current namespace, nor imported"
			let validNS	= head validNSes
			(fqname, ld, ())	<- explicitImport dots dots scope validNS
			return ((fqname, name), getDict ld ! name)
resolve' scope (entity, getWhole, getPart) (ns, name)
	= do	(fqname, a, ())	<- explicitImport dots dots scope ns
		let known	= a & getWhole & maybe M.empty getPart
		assert (name `M.member` known) $ uppercase entity ++" "++ show name++" was not found within "++dots ns++" (which resolves to "++dots fqname++")"
		return ((fqname, name), known ! name)




		
-- Gets the imports that are available without explicit qualification, based on the flags
implicitImports	:: Scope name nameInt a () () -> Map name a
implicitImports scope
	= scope & get imported |> fst





instance Check LangDefs where
	check (LangDefs defs)
		= do	let syntaxes	= defs |> get (payload . langSyntax) & M.mapMaybe id
			check syntaxes
