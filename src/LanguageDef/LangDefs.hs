{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
module LanguageDef.LangDefs where

{-

Definition of a cluster language definitions.

See also: LanguageDef.LangDefsFix

-}

import Utils.All

import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.Checkable

import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.Grouper

import LanguageDef.Data.BNF (BNF, overRuleCall', getRuleCall)
import LanguageDef.Data.SyntacticForm
import LanguageDef.Data.ParseTree
import LanguageDef.Data.LanguageDef
import LanguageDef.Data.Expression
import LanguageDef.Data.SyntFormIndex
import LanguageDef.Data.Function
import LanguageDef.Data.Rule
import LanguageDef.Data.Relation
import LanguageDef.MetaSyntax (typeTop, typeBottom)
import qualified LanguageDef.Builtins as Builtins


import Graphs.Lattice (makeLattice, Lattice, debugLattice)

import Control.Arrow ((&&&))
import Data.Bifunctor (first)
import Data.Function (on)

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
	{ _ldScope	:: LanguageDef' ResolvedImport funcResolution
	, _imported	:: Map [Name] ImportFlags
	, _environment	:: Map [Name] (LDScope' funcResolution)	-- All loaded modules
	} 
	deriving (Show, Eq)
makeLenses ''LDScope'
type LDScope	=  LDScope' SyntFormIndex

instance ToString (LDScope' fr) where
	toParsable ldscope	= get ldScope ldscope & toParsable


updateEnv	:: (LanguageDef' ResolvedImport fr0, Map [Name] (LDScope' fr0)) -> LDScope' fr -> LDScope' fr0
updateEnv (scope, env) (LDScope _ imps _)
	= LDScope scope imps env


enterScope	::  [Name] -> LDScope' fr -> Failable (LDScope' fr)
enterScope names lds
	= do	let allScopes	= get environment lds
		let dist	= (dots, levenshtein `on` last)	:: ([Name] -> String, [Name] -> [Name] -> Int)
		checkExistsSuggDist' dist names (get environment lds)
			("The scope "++dots names++" was not found")
		

{- | Parses a target string according to the language definition cluster

>>> import LanguageDef.API
>>> parseTarget testLanguage (["TestLanguage"], "bool") "?"  "True"
Success (RuleEnter {_pt = Literal {_ptToken = "True", ...}, _ptUsedRule = (["TestLanguage"],"bool"), _ptUsedIndex = 0, ...)


-}
parseTarget	:: LDScope' fr -> FQName -> FilePath -> String -> Failable ParseTree
parseTarget langs (startModule, startRule) file contents
	= do	let syntaxes	= langs & get environment |> get ldScope |> get langSyntax |> fromMaybe emptySyntax
		parse file (syntaxes, startModule) startRule contents 



fullyQualifySyntForm
			:: LDScope' fr -> SyntacticForm -> Failable SyntacticForm
fullyQualifySyntForm scope (SyntacticForm nm choices meta docs)
	= inMsg' ("While fully qualifiying the syntactic form "++show nm) $ inLocation (get miLoc docs) $
	  do	choices'	<- choices |> fullyQualifyBNF scope & allGood
		return $ SyntacticForm nm choices' meta docs


fullyQualifyBNF	:: LDScope' fr -> BNF -> Failable BNF
fullyQualifyBNF scope bnf
	= inMsg' ("While fully qualifiying the BNF "++toParsable bnf) $
		bnf & overRuleCall' (resolve scope syntaxCall)


-- Qualifies all function types absolutely
fullyQualifyFunction	:: LDScope' fr -> Function' a -> Failable (Function' a)
fullyQualifyFunction scope (Function nm argTps retType clauses docs)
	= inMsg' ("While fully qualifiying the function "++show nm) $ inLocation (get miLoc docs) $ 
	  do	argTps'	<- argTps |> resolve scope syntaxCall & allGood
		retType'	<- retType & resolve scope syntaxCall
		clauses'	<- clauses |> fullyQualifyClause scope & allGood
		return $ Function nm argTps' retType' clauses' docs


fullyQualifyClause	:: LDScope' fr -> FunctionClause' a -> Failable (FunctionClause' a)
fullyQualifyClause _	= return	-- full qualification happens by the typer



fullyQualifyRelation	:: LDScope' fr -> Relation -> Failable Relation
fullyQualifyRelation scope rel
	= inMsg' ("While fully qualifiying the relation form "++show (get relSymbol rel)) $ inLocation (get (relDocs . miLoc) rel) $ 
	  do	let tps	= get relTypes rel
		tps'	<- tps |> first (resolve scope syntaxCall) |+> fstEffect
		return $ set relTypes tps' rel



fullyQualifyRule	:: LDScope' fr -> Rule' fr -> Failable (Rule' fr)
fullyQualifyRule scope (Rule preds concl name docs)
	= inMsg' ("While fully qualifiying the rule "++show name) $ inLocation (get miLoc docs) $ 
	  do	concl'	<- fullyQualifyConcl scope concl
		preds'	<- preds |> fullyQualifyPred scope & allGood
		return $ Rule preds' concl' name docs


fullyQualifyPred	:: LDScope' fr -> Predicate' a -> Failable (Predicate' a)
fullyQualifyPred scope (PredConcl concl li)
	= do	concl'	<- fullyQualifyConcl scope concl
		return $ PredConcl concl' li
fullyQualifyPred scope (PredExpr expr li)
	= return $ PredExpr expr li	-- should be typechecked


fullyQualifyConcl	:: LDScope' fr -> Conclusion' a -> Failable (Conclusion' a)
fullyQualifyConcl scope (Conclusion relName args)
	= inMsg' ("While fully qualifiying a conclusion using "++show relName) $ inLocation (get expLocation $ head args ) $ 
	  do	let args'	= args	-- qualification of expressions is done by the typechecker
		relName'	<- resolve scope relationCall relName
		return $ Conclusion relName' args'


type Resolver' fr x
		= (String, LanguageDef' ResolvedImport fr -> Maybe (Grouper x), [(FQName, x)])
type Resolver x
		= Resolver' SyntFormIndex x



syntaxCall	:: Resolver' fr SyntacticForm
syntaxCall	= ("the syntactic form", get langSyntax, Builtins.syntaxExtras)

functionCall	:: Resolver' fr (Function' fr)
functionCall	= ("the function", get langFunctions, [])


relationCall	:: Resolver' fr Relation
relationCall	= ("the relation", get langRelations, [])

ruleCall	:: Resolver' fr (Rule' fr)
ruleCall	= ("the rule", get langRules, [])


resolveGlobal	:: Eq x => LDScope  -> Resolver x-> FQName -> Failable (FQName, x)
resolveGlobal lds entity fqname
	= do	let path	= fst fqname
		ld	<- checkExistsSuggDist' (dots, levenshtein `on` last) path (get environment lds) 
				("Namespace "++ dots path ++ " was not found")
		resolve' ld entity fqname

resolve		:: Eq x => LDScope' fr -> Resolver' fr x-> FQName -> Failable FQName
resolve	scope resolver fqn
	= resolve' scope resolver fqn |> fst

resolve_ scope resolver fqn
	= resolve' scope resolver fqn |> snd

resolve'	:: Eq x => LDScope' fr ->  Resolver' fr x -> FQName -> Failable (FQName, x)
resolve' scope resolver fqn
	= do	let resDict	= resolutionMap scope resolver
		let name	= over _head toUpper (fst3 resolver) ++ " " ++ show (showFQ fqn)
		results	<- checkExistsSuggDist' distFQ fqn resDict (name ++ " was not found within the namespace "++dots (fst fqn))
		assert' (length results == 1) $ name ++ " could resolve to multiple entities:\n"++(results |> fst |> showFQ & unlines & indent)
		return $ head results

		
{- Creates a dict {this local name --> these possible entities} -}
resolutionMap	:: Eq x => LDScope' fr -> Resolver' fr x -> Map FQName [(FQName, x)]
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
The returning format is: 
	- fully qualified name
	- is locally defined (and has resolution priority)
	- locally known as names
	- actual entity 
Each fqname should also occur under the 'known as'

-}
allKnowns	:: LDScope' fr -> Resolver' fr x -> [((FQName, Bool), [FQName], x)]
allKnowns scope resolver	
	= let	-- The imports contain the local scope as well, so we don't need _allKnownLocally here
		imports		= get imported scope & M.toList		:: [([Name], ImportFlags)]
		env		= get environment scope |> get ldScope	-- :: Map [Name] (LanguageDef' ResolvedImport fr) 
		mergedImports	= imports |> _mergeImport env resolver
		-- TODO extra, renamed imports 
		-- TODO include re-exports
		in
		concat mergedImports


_mergeImport	:: Map [Name] (LanguageDef' ResolvedImport fr) -> Resolver' fr b -> ([Name], ImportFlags) -> [((FQName, Bool), [FQName], b)] 
_mergeImport scopes resolver (fq, ImportFlags _ isSelf knownNames)
	= let	ld	= scopes ! fq
		found	= _allKnownLocally (fq, ld) resolver 
		in
		found |> (\(fqn, b) -> ((fqn, isSelf), knownNames |> flip (,) (snd fqn), b))


_allKnownLocally	:: ([Name], LanguageDef' ResolvedImport fr) -> Resolver' fr b -> [(FQName, b)]
_allKnownLocally (fq, ld) (_, getWhole, extras)
	= let	dict	= ld 	& getWhole |> get grouperDict
				& maybe [] M.toList
		in
		extras ++ dict |> first ((,) fq)
		
	


instance Checkable' [Name] LDScope where
	check' fq ldscope	
		= do	let ld		= get ldScope ldscope
			let isSubtype	= ld & isSubtypeOf
			let resolveSF	= resolve ldscope syntaxCall
			let extras	= (resolveSF , isSubtype, fq)	:: (FQName -> Failable FQName, FQName -> FQName -> Bool, [Name])
			[check' extras ld, _checkSameTitle fq ld] & allGood
			pass

{- |
>>> import LanguageDef.API
>>> loadAssetLangDef "TestInput/Faulty" ["TitleMismatch"] & toCoParsable
"| While validating \nError: \n  \8226 The module in file TitleMismatch is titled \"Some Other Title\". Retitle them to be the same (whitespace insensitive)"
-}

_checkSameTitle	:: [Name] -> LanguageDef -> Check
_checkSameTitle fq ld
	= do	let nm	= get langTitle ld
		let noWS string	= string & L.filter (`notElem` " \t")
		let msg	= "The module in file "++dots fq ++" is titled "++show nm++". Retitle them to be the same (whitespace insensitive)"
		assert' (noWS (last fq) == noWS nm) msg

