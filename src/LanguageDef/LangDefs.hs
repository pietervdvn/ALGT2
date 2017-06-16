module LanguageDef.LangDefs where


import Utils.All


import LanguageDef.LanguageDef
import LanguageDef.LocationInfo
import LanguageDef.Syntax.All
import LanguageDef.Syntax.BNF (overRuleCall')
import LanguageDef.Scope

import Control.Arrow ((&&&))

import Data.Maybe
import Data.Map as M
import Data.List as L


type LDScope	= Scope [Name] [Name] LanguageDef ({-Import flags-}) ({-Re-export flags-})

{- Contains the full cluster of language defintions + checks -}
data LangDefs	= LangDefs {_langdefs	:: Map [Name] LDScope}
	deriving (Show)

asLangDefs		:: Map [Name] LanguageDef -> Either String LangDefs
asLangDefs defs	= do	scopes		<- defs & M.toList |> (fst &&& _scopeFor defs) |+> sndEffect
			scopes'		<- scopes ||>> fixScope |+> sndEffect
			let ld		= scopes' & M.fromList & knotScopes dots & LangDefs
			check ld
			return ld



	

_scopeFor	:: Map [Name] LanguageDef -> ([Name], LanguageDef) -> Either String LDScope
_scopeFor ldefs (fqname, ld)
	= do	let imports	= ld & get langImports |> get importName
		let units	= repeat ()
		imported	<- imports |+> (\k -> checkExists k ldefs ("Weird, import "++show k++" not found... Bug in LangDefs"))
		return $ Scope
			fqname
			(zip imports (zip imported units) & M.fromList)	-- imported
			(zip imports imports & M.fromList)	-- internalView
			ld					-- actual payload/exports; resolve will filter
			M.empty					-- reExports


-- Prepares the language definitions for production (e.g. resolves all calls to be fully qualified). The first argument should be a dict with all the fully fixed scopes
fixScope	:: LDScope -> Either String LDScope
fixScope scopeToFix
	= do	let ld	= get payload scopeToFix
		ld'	<- fixLD scopeToFix ld
		scopeToFix	& set payload ld'
				& return


fixLD		:: LDScope -> LanguageDef -> Either String LanguageDef
fixLD scope ld
	= do	let syn		= get baseSyntax ld
		syn'		<- syn |> fixSyntax scope & justEffect
		return $ set baseSyntax syn' ld

fixSyntax	:: LDScope -> Syntax -> Either String Syntax
fixSyntax scope syn
	= do	let syntx	= get syntax syn
		syntx'		<- syntx & M.toList
					|> (fst &&& fullyQualifySyntForm scope)
					|+> sndEffect |> M.fromList
		return $ set syntax syntx' syn

fullyQualifySyntForm
		:: LDScope -> (Name, [(BNF, MetaInfo)]) -> Either String [(BNF, MetaInfo)]
fullyQualifySyntForm scope (syntFormName, choices)
	= inMsg ("In the definition of "++syntFormName) $
		choices |> (fullyQualifyBNF scope &&& snd) |+> fstEffect


fullyQualifyBNF	:: LDScope -> (BNF, MetaInfo) -> Either String BNF
fullyQualifyBNF scope (bnf, metaInfo)
	= inMsg ("In a choice "++toCoParsable (get miLoc metaInfo)) $
		bnf & overRuleCall' (scope & resolve syntaxCall)



syntaxCall	:: (String, LanguageDef -> Maybe Syntax, Syntax -> Map Name [(BNF, MetaInfo)])
syntaxCall	= ("the syntactic form", get baseSyntax, get syntax)

resolve	:: (String, LanguageDef -> Maybe a, a -> Map Name b) -> LDScope -> FQName -> Either String FQName
resolve (entity, getWhole, getPart) scope ([], name)
	-- the localscope/direct import case
	= do	let getDict ld	= ld & getWhole & maybe M.empty getPart
		let getKnown ld	= getDict ld & keys	:: [Name]
		if name `elem` (scope & get payload & getKnown) then
			-- we found it locally
			return (get scopeName scope, name)
		else do
			-- Lets take a look at the imports
			let validNSes	= 
				implicitImports scope |> getKnown	-- { [Name] (path) --> [Name] (known syntactic forms, of which we search one }
				& M.filter (name `elem`)	-- Name is an element of the knownNames
				& M.keys			-- These namespaces export an element called `name`
			assert (length validNSes < 2) $
				["Multiple namespaces export "++entity++" "++show name++", namely: "
				, validNSes |> intercalate "." & unlines & indent
				, "Add a prefix to disambiguate the exact object you want"
				] & unlines
			assert (not $ L.null validNSes) $ uppercase entity ++" "++ show name ++ " was not found. It is not defined in the current namespace, nor imported"
			return (head validNSes, name)
resolve (entity, getWhole, getPart) scope (ns, name)
	= do	(fqname, a, ())	<- explicitImport dots dots scope ns
		let known	= a & getWhole & maybe M.empty getPart
		assert (name `M.member` known) $ uppercase entity ++" "++ show name++" was not found within "++dots ns++" (which resolves to "++dots fqname++")"
		return (fqname, name)


		
-- Gets the imports that are available without explicit qualification, based on the flags
implicitImports	:: Scope name nameInt a () () -> Map name a
implicitImports scope
	= scope & get imported |> fst





instance Check LangDefs where
	check (LangDefs defs)
		= do	let syntaxes	= defs |> get (payload . baseSyntax) & M.mapMaybe id
			check syntaxes
