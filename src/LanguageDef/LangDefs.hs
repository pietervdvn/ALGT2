{-# LANGUAGE TemplateHaskell #-}
module LanguageDef.LangDefs where

{-

Definition of a cluster language definitions.

See also: LanguageDef.LangDefsFix

-}

import Utils.All


import LanguageDef.LanguageDef
import LanguageDef.LocationInfo
import LanguageDef.Grouper
import LanguageDef.Syntax.All
import LanguageDef.Syntax.BNF (overRuleCall', getRuleCall)
import LanguageDef.Scope
import LanguageDef.Expression
import LanguageDef.Function
import LanguageDef.Rule
import LanguageDef.Relation


import Graphs.Lattice (makeLattice, Lattice, debugLattice)

import Control.Arrow ((&&&))
import Data.Bifunctor (first)

import Data.Maybe
import Data.Set as S
import Data.Map as M
import Data.List as L


data ImportFlags = ImportFlags
	{ _ifOrigin	:: FilePath
	, _ifIsSelf	:: Bool		-- If true: this is the namespace itself, and thus has priority
	, _ifDiffName	:: [[Name]]	-- Reachable with these names
	} deriving (Show, Ord, Eq)
makeLenses ''ImportFlags

data LDScope' funcResolution = LDScope
	{ _ldScope	:: Scope [Name] [Name] (LanguageDef' ResolvedImport funcResolution) ImportFlags ({-Re-export flags-})
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


type Resolver fr x
		= (String, LanguageDef' ResolvedImport fr -> Maybe (Grouper x), Grouper x -> Map Name x)

syntaxCall	:: Resolver fr SyntacticForm
syntaxCall	= ("the syntactic form", get langSyntax, get grouperDict)

functionCall	:: Resolver fr (Function' fr)
functionCall	= ("the function", get langFunctions, get grouperDict)


relationCall	:: Resolver fr Relation
relationCall	= ("the relation", get langRelations, get grouperDict)

ruleCall	:: Resolver fr (Rule' fr)
ruleCall	= ("the rule", get langRules, get grouperDict)


resolveGlobal	:: Eq x => LangDefs -> (String, LanguageDef -> Maybe (Grouper x), (Grouper x) -> Map Name x) -> FQName -> Either String (FQName, x)
resolveGlobal lds entity fqname
	= do	ld	<- checkExistsSugg show (fst fqname) (lds & get langdefs) ("Namespace "++ dots (fst fqname) ++ " was not found")
		resolve' ld entity fqname

resolve	:: Eq x => LDScope' fr -> Resolver fr x-> FQName -> Either String FQName
resolve	scope resolver fqn
	= resolve' scope resolver fqn |> fst

resolve'	:: Eq x => LDScope' fr ->  Resolver fr x -> FQName -> Either String (FQName, x)
resolve' scope resolver fqn
	= do	let resDict	= resolutionMap scope resolver
		let name	= over _head toUpper (fst3 resolver) ++ " " ++ show (showFQ fqn)
		results	<- checkExists fqn resDict (name ++ " was not found.")
		assert (length results == 1) $ name ++ " could resolve to multiple entities:\n"++(results |> fst |> showFQ & unlines & indent)
		return $ head results
		
{- Creates a dict {this local name --> these possible entities} -}
resolutionMap	:: Eq x => LDScope' fr -> Resolver fr x -> Map FQName [(FQName, x)]
resolutionMap scope resolver
	= let	preDict	= allKnowns scope resolver |> (\(entity, knownAs, x) -> [(ka, (entity, x)) | ka <- knownAs] )
				& concat
				-- :: [ (FQName, ((FQName, Bool), x))]
		in
		preDict & merge & M.fromList |> pickPriority |> nub

pickPriority	:: [((a, Bool), b)] -> [(a, b)]
pickPriority list
	= let	(prior, nonPrior)	= list |> unmerge3l & L.partition snd3
		in
		(if L.null prior then nonPrior else prior) |> dropSnd3

{- | Gives all items that are known for a certain resolver (e.g. all available syntaxCalls etc...
The returning format is: fully qualified name, locally known as names, actual entity 
Each fqname should also occur under the 'known as'

-}
allKnowns	:: LDScope' fr -> Resolver fr x -> [((FQName, Bool), [FQName], x)]
allKnowns scope resolver	
	= let	-- The imports contain the local scope as well, so we don't need _allKnownLocally here
		imports		= get (ldScope . imported) scope & M.toList		:: [([Name], ImportFlags)]
		mergedImports	= imports |> _mergeImport (get environment scope) resolver
		-- TODO extra, renamed imports 
		-- TODO include re-exports
		in
		concat mergedImports


_mergeImport	:: Map [Name] (LanguageDef' ResolvedImport fr) -> Resolver fr b -> ([Name], ImportFlags) -> [((FQName, Bool), [FQName], b)] 
_mergeImport scopes resolver (fq, ImportFlags _ isSelf knownNames)
	= let	ld	= scopes ! fq
		found	= _allKnownLocally (fq, ld) resolver 
		in
		found |> (\(fqn, b) -> ((fqn, isSelf), knownNames |> flip (,) (snd fqn), b))


_allKnownLocally	:: ([Name], LanguageDef' ResolvedImport fr) -> Resolver fr b -> [(FQName, b)]
_allKnownLocally (fq, ld) (_, getWhole, _)
	= let	dict	= ld 	& getWhole |> get grouperDict
				& maybe [] M.toList
		in
		dict |> first ((,) fq)
		
	
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

