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
			:: LDScope' fr -> (Name, [(BNF, MetaInfo)]) -> Either String [(BNF, MetaInfo)]
fullyQualifySyntForm scope (syntFormName, choices)
	= inMsg ("In the definition of "++syntFormName) $
		choices |> (fullyQualifyBNF scope &&& snd) |+> fstEffect


fullyQualifyBNF	:: LDScope' fr -> (BNF, MetaInfo) -> Either String BNF
fullyQualifyBNF scope (bnf, metaInfo)
	= inMsg ("In a choice "++toCoParsable (get miLoc metaInfo)) $
		bnf & overRuleCall' (resolve scope syntaxCall)


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


syntaxCall	:: (String, LanguageDef' ResolvedImport fr -> Maybe Syntax, Syntax -> Map Name [BNF])
syntaxCall	= ("the syntactic form", get langSyntax, \synt -> get syntax synt ||>> fst)

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



getSyntaxes	:: LangDefs -> Syntaxes
getSyntaxes (LangDefs defs)
	= defs |> get (ldScope . payload . langSyntax) & M.mapMaybe id
			
{- |
>>> import LanguageDef.API
>>> loadAssetLangDef "Faulty" ["TitleMismatch"]
Left "The module in file TitleMismatch is titled \"Some Other Title\". Retitle them to be the same (whitespace insensitive)"
-}
instance Check LangDefs where
	check lds@(LangDefs defs)
		= do	let syntaxes	= getSyntaxes lds
			let isSubtype nms	= defs & (! nms) & get (ldScope . payload) & isSubtypeOf
			[ defs & M.elems |> get (ldScope . payload) 
				|> check' ()
				& allRight_
			 , check' isSubtype syntaxes
			 , defs & M.toList ||>> get (ldScope . payload . langTitle)
				 |> uncurry _checkSameTitle & allRight_
			 ] & allRight_

_checkSameTitle	:: [Name] -> Name -> Either String ()
_checkSameTitle fp nm
	= do	let noWS string	= string & L.filter (`notElem` " \t")
		let msg	= "The module in file "++dots fp ++" is titled "++show nm++". Retitle them to be the same (whitespace insensitive)"
		assert (noWS (last fp) == noWS nm) msg

instance ToString LangDefs where 
	toParsable ld	= ld & get langdefs & M.toList |> _withHeader & unlines


_withHeader (nm, ld)
	= ["\n---------------", dots nm, "-------------\n"] & unwords ++ (ld & get (ldScope . payload) & toParsable)

