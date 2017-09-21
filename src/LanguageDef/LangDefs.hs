{-# LANGUAGE TemplateHaskell #-}
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
import LanguageDef.Rule
import LanguageDef.Relation
import LanguageDef.MetaExpression

import LanguageDef.Grouper

import Graphs.Lattice (makeLattice, Lattice, debugLattice)

import Control.Arrow ((&&&))
import Data.Bifunctor (first)

import Data.Maybe
import Data.Set as S
import Data.Map as M
import Data.List as L


data LDScope' funcResolution = LDScope
	{ _ldScope	:: Scope [Name] [Name] (LanguageDef' ResolvedImport funcResolution) ({-Import flags-}) ({-Re-export flags-})
	, _environment	:: Map [Name] (LanguageDef' ResolvedImport funcResolution)
	} 
	deriving (Show, Eq)
makeLenses ''LDScope'
type LDScope	=  LDScope' SyntFormIndex

{- Contains the full cluster of language defintions -}
data LangDefs	= LangDefs {_langdefs	:: Map [Name] LDScope}
	deriving (Show)
makeLenses ''LangDefs

langDef	:: LangDefs -> [Name] -> Maybe LanguageDef
langDef defs fq
	= do	def'	<- M.lookup fq (get langdefs defs)
		def' & get (ldScope . payload) & return


-- Re-updates the environment of an LDScope
knotScopes :: Map [Name] (LDScope' fr) -> Map [Name] (LDScope' fr)
knotScopes lds
	= let	env	= lds |> get (ldScope . payload) in
		lds |> set environment env


{- | Parses a target string according to the language definition cluster

>>> import AssetUtils
>>> parseTarget testLangDefs (["TestLanguage"], "bool") "?"  "True"
Right (RuleEnter {_pt = Literal {_ptToken = "True", ...}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ...)


-}
parseTarget	:: LangDefs -> FQName -> FilePath -> String -> Either String ParseTree'
parseTarget langs (startModule, startRule) file contents
	= let	syntaxes	= langs & get langdefs |> get (ldScope . payload) |> get langSyntax |> fromMaybe emptySyntax
		in
		parse file (syntaxes, startModule) startRule contents



fullyQualifySyntForm
			:: LDScope' fr -> SyntacticForm -> Either String SyntacticForm
fullyQualifySyntForm scope (SyntacticForm nm choices meta docs)
	= inMsg ("In the definition of "++nm) $
	   do	choices'	<- choices |> fullyQualifyBNF scope & allRight'
		return $ SyntacticForm nm choices' meta docs


fullyQualifyBNF	:: LDScope' fr -> BNF -> Either String BNF
fullyQualifyBNF scope bnf
	= bnf & overRuleCall' (resolve scope syntaxCall)


-- Qualifies all function types absolutely
fullyQualifyFunction	:: LDScope' fr -> Function' a -> Either String (Function' a)
fullyQualifyFunction scope (Function nm argTps retType clauses docs)
	= do	argTps'	<- argTps |> resolve scope syntaxCall & allRight'
		retType'	<- retType & resolve scope syntaxCall
		clauses'	<- clauses |> fullyQualifyClause scope & allRight'
		return $ Function nm argTps' retType' clauses' docs


fullyQualifyClause	:: LDScope' fr -> FunctionClause a -> Either String (FunctionClause a)
fullyQualifyClause _	= return	-- full qualification happens by the typer



fullyQualifyRelation	:: LDScope' fr -> Relation -> Either String Relation
fullyQualifyRelation scope rel
	= do	let tps	= get relTypes rel
		tps'	<- tps |> first (resolve scope syntaxCall) |+> fstEffect
		return $ set relTypes tps' rel



fullyQualifyRule	:: LDScope' fr -> Rule -> Either String Rule
fullyQualifyRule scope (Rule preds concl name docs)
	= do	concl'	<- fullyQualifyConcl scope concl
		preds'	<- preds |> fullyQualifyPred scope & allRight
		return $ Rule preds' concl' name docs


fullyQualifyPred	:: LDScope' fr -> Either (Conclusion a) (Expression a) -> Either String (Either (Conclusion a) (Expression a))
fullyQualifyPred scope (Left concl)
	= fullyQualifyConcl scope concl |> Left
fullyQualifyPred scope (Right expr)
	= return $ Right expr	-- should be typechecked


fullyQualifyConcl	:: LDScope' fr -> Conclusion a -> Either String (Conclusion a)
fullyQualifyConcl scope (Conclusion relName args)
	= do	let args'	= args	-- qualification of expressions is done by the typechecker
		relName'	<- resolve scope relationCall relName
		return $ Conclusion relName' args'


syntaxCall	:: (String, LanguageDef' ResolvedImport fr -> Maybe Syntax, Syntax -> Map Name SyntacticForm)
syntaxCall	= ("the syntactic form", get langSyntax, get grouperDict)

functionCall	:: (String, LanguageDef' ResolvedImport fr -> Maybe (Grouper (Function' fr)), Grouper (Function' fr) -> Map Name (Function' fr))
functionCall	= ("the function", get langFunctions, get grouperDict)


relationCall	= ("the relation", get langRelations, get grouperDict)

ruleCall	:: (String, LanguageDef' ResolvedImport fr -> Maybe (Grouper Rule), Grouper Rule -> Map Name Rule)
ruleCall	= ("the rule", get langRules, get grouperDict)


resolveGlobal	:: LangDefs -> (String, LanguageDef -> Maybe a, a -> Map Name b) -> FQName -> Either String (FQName, b)
resolveGlobal lds entity fqname
	= do	ld	<- checkExistsSugg show (fst fqname) (lds & get langdefs) ("Namespace "++ dots (fst fqname) ++ " was not found")
		resolve' ld entity fqname

resolve	:: LDScope' fr -> (String, LanguageDef' ResolvedImport fr -> Maybe a, a -> Map Name b) -> FQName -> Either String FQName
resolve	scope entity name
	= resolve' scope entity name |> fst

resolve'	:: LDScope' fr ->  (String, LanguageDef' ResolvedImport fr -> Maybe a, a -> Map Name b) -> FQName -> Either String (FQName, b)
resolve' scope (entity, getWhole, getPart) ([], name)
	-- the localscope/direct import case
	= do	let getDict ld	= ld & getWhole & maybe M.empty getPart
		let getKnown ld	= getDict ld & keys	:: [Name]
		let ld		= scope & get (ldScope . payload)
		if name `elem` getKnown ld then
			-- we found it locally
			return ((get (ldScope . scopeName) scope, name), getDict ld ! name)
		else do
			-- Lets take a look at the imports
			let foundNSes	= 
				_implicitImports scope |> getKnown	-- { [Name] (path) --> [Name] (known syntactic forms, of which we search one }
					& M.filter (name `elem`)	-- Name is an element of the knownNames
			let validNSes	= foundNSes & M.keys		-- These namespaces export an element called `name`
			assert (length validNSes < 2) $
				["Multiple namespaces export "++entity++" "++show name++", namely: "
				, validNSes |> intercalate "." & unlines & indent
				, "Add a prefix to disambiguate the exact object you want"
				] & unlines
			assert (not $ L.null validNSes) $ uppercase entity ++" "++ show name ++ " was not found. It is not defined in the current namespace, nor imported"
			let validNS	= head validNSes
			(fqname, ld, ())	<- _explicitImport scope validNS
			return ((fqname, name), getDict ld ! name)
resolve' scope (entity, getWhole, getPart) (ns, name)
	= do	(fqname, a, ())	<- _explicitImport scope ns
		let known	= a & getWhole & maybe M.empty getPart
		assert (name `M.member` known) $ uppercase entity ++" "++ show name++" was not found within "++dots ns++" (which resolves to "++dots fqname++")"
		return ((fqname, name), known ! name)


_explicitImport ldscope
	= explicitImport dots dots (get environment ldscope) (get ldScope ldscope)

		
-- Gets the imports that are available without explicit qualification (thus "bool" instead of "Langs.STFL.bool"), based on the import flags
-- TODO for now, import flags don't do anything. Fix it!
_implicitImports :: LDScope' a -> Map [Name] (LanguageDef' ResolvedImport a)
_implicitImports (LDScope scope env)
	= importsFromEnv env scope


	
{- |
>>> import LanguageDef.API
>>> loadAssetLangDef "Faulty" ["TitleMismatch"]
Left "The module in file TitleMismatch is titled \"Some Other Title\". Retitle them to be the same (whitespace insensitive)"
-}
instance Checkable LangDefs where
	check lds@(LangDefs defs)
		= defs & M.toList |> _checkOne & allRight_
	

-- Check' (FQName -> Either String FQName, [Name] -> FQName -> FQName -> Bool, [Name])

_checkOne	:: ([Name], LDScope) -> Check
_checkOne (fq, ldscope)
	= do	let ld		= get (ldScope . payload) ldscope
		let isSubtype	= ld & isSubtypeOf
		let resolveSF	= resolve ldscope syntaxCall
		let extras	= (resolveSF , isSubtype, fq)	:: (FQName -> Either String FQName, FQName -> FQName -> Bool, [Name])
		check' extras ld <> _checkSameTitle fq ld

_checkSameTitle	:: [Name] -> LanguageDef -> Check
_checkSameTitle fq ld
	= do	let nm	= get langTitle ld
		let noWS string	= string & L.filter (`notElem` " \t")
		let msg	= "The module in file "++dots fq ++" is titled "++show nm++". Retitle them to be the same (whitespace insensitive)"
		assert (noWS (last fq) == noWS nm) msg

instance ToString LangDefs where 
	toParsable ld	= ld & get langdefs & M.toList |> _withHeader & unlines


_withHeader (nm, ld)
	= ["\n---------------", dots nm, "-------------\n"] & unwords ++ (ld & get (ldScope . payload) & toParsable)

