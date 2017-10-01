{-# LANGUAGE TemplateHaskell, DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}
module LanguageDef.LanguageDef where

{-

A language defintion builds on many language-definition aspects. Each language definition aspect has its own section, for which it handles the parsing (and meta-data such as documentation)

 -}

import Utils.All

import qualified LanguageDef.Syntax.BNF as BNF
import LanguageDef.Syntax.All
import LanguageDef.LocationInfo
import LanguageDef.Expression hiding (choices')
import LanguageDef.Function hiding (choices')
import LanguageDef.Relation hiding (choices')
import LanguageDef.Rule
import qualified LanguageDef.Relation as Relations
import LanguageDef.Grouper

import Graphs.Lattice

import Data.Char
import Data.List as L

import Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Maybe
import Data.Bifunctor (first)
import Control.Arrow ((&&&), (***))
import Control.Monad

import System.Directory

import qualified Assets


data Import a = Import
	{ _importName	:: [Name]
	, _isLocal	:: Bool
	, _importMeta	:: MetaInfo
	, _importA	:: a
	} deriving (Show, Eq, Functor)
makeLenses ''Import

{- | The main definition of a language. Represents an entire langdef file.
The extra type arguments are used to indicate the level of resolution of the definition:
- LanguageDef (indicates resolution of the imports) (indicates resolution of expressions/functions)

-}
data LanguageDef' imported funcResolution
	 = LanguageDef 
		{ _langTitle	:: Name		-- title of the language
		, _langImports	:: [Import imported]
		, _langMeta	:: [String]	-- The comments just under the title
		, _langLocation	:: LocationInfo
		, _langSyntax		:: Maybe (Grouper SyntacticForm)	-- The syntax of the language, aka the BNF
		, _langSupertypes	:: Lattice FQName	-- The global supertype relationship; is filled in later on by the langdefs-fixes
		, _langFunctions	:: Maybe (Grouper (Function' funcResolution))
		, _langRelations	:: Maybe (Grouper Relation)
		, _langRules		:: Maybe (Grouper (Rule' funcResolution))
			-- TODO typecheck the rules!
		}
	deriving (Show, Eq)
makeLenses ''LanguageDef'


updateFR	:: (Maybe (Grouper (Function' fr1)), Maybe (Grouper (Rule' fr1))) -> LanguageDef' imported fr0 -> LanguageDef' imported fr1
updateFR (funcs, rules) (LanguageDef title imps meta loc synt supers _ rels _)
	= LanguageDef title imps meta loc synt supers funcs rels rules

type ResolvedImport	= FilePath
type LanguageDef	= LanguageDef' ResolvedImport SyntFormIndex



{- | Performs various checks. These checks are run post name resolution, so we might assume that no unqualified names still exists


>>> import LanguageDef.API
>>> loadAssetLangDef "Faulty" ["FunctionDuplicateNameTest"]
Left "The function \"not\" is defined multiple times"

>>> loadAssetLangDef "Faulty" ["FunctionIncorrectNameTest"]
Left "While checking function \"not\":\n  Some clauses have a different name. The function name is \"not\", but a clause is named f"

-}

instance Checkable' (FQName -> Either String FQName, FQName -> FQName -> Bool, [Name]) (LanguageDef' ResolvedImport SyntFormIndex) where
	check' extras (LanguageDef title imports meta li syntax superTypes functions rels rules)
		= do	assert (title /= "") "The title of a language should not be empty"
			checkM' extras syntax
			checkM functions
			checkM rels
			assert (isNothing rules || isJust rels) "When rules are defined, a relation declaration section should be present"
			checkM' (fromJust rels) rules
			
typeBottom	= ([], "⊥")
typeTop		= ([], "⊤")


isSubtypeOf	:: LanguageDef' ResolvedImport fr -> FQName -> FQName -> Bool
isSubtypeOf ld	sub super
 | sub == super	= True
 | otherwise
	= isSubsetOf (get langSupertypes ld) sub super

-------------------------------- IMPORT FIXING STUFF ------------------------------------


resolveLocalImports	:: ([Name], [Name]) -> LanguageDef' x f -> LanguageDef' x f
resolveLocalImports (offsetForAll, extraOffsetForLocal)
	= let	fixImport imp	= if get isLocal imp 
					then imp & set isLocal False & over importName ((offsetForAll ++ extraOffsetForLocal) ++ )
					else imp & over importName (offsetForAll ++)
		in
		over (langImports . mapped) fixImport

fixImport		:: Map [Name] FilePath -> LanguageDef' () fr -> Either String (LanguageDef' ResolvedImport fr)  
fixImport resolver ld
	= inMsg ("While fixing the import annotations for "++show (get langTitle ld)) $
          do	let imps	= get langImports ld
		imps'		<- imps |> _fixImport resolver & allRight
		return $ set langImports imps' ld 
		

_fixImport		:: Map [Name] FilePath -> Import () -> Either String (Import FilePath)
_fixImport resolver imprt
	= do	let nm		= get importName imprt	:: [Name]
		resolved	<- checkExists nm resolver ("The import for "++intercalate "." nm++" was not found")
		return (imprt |> const resolved)


------------------------------ PARSING STUFF --------------------------------------------------------

_checkCombiner	= check' metaSyntaxes (parseLangDef _fullFileCombiner) & either error id

-- | Parses the entire file, file should still be checked against it's context!
-- >>> _checkCombiner
-- ()
-- >>> parseFullFile ["TestLang"] "Test:Assets/TestLang" Assets._TestLanguage_language 
-- Right ...
parseFullFile	:: [Name] -> FilePath -> String -> Either String (LanguageDef' () ())
parseFullFile _ fp contents
	= inMsg ("While parsing "++show fp) $
	  do	pt	<- parse fp (metaSyntaxes, ["ALGT"]) "langDef" contents
		
		(li, langDef)	<- interpret (parseLangDef _fullFileCombiner & withLocation (,)) pt
		let ((title, meta, imports), (syntax, (funcs, (rels, rules))))	= langDef	
		return $ LanguageDef
			title
			imports
			meta
			li
			syntax
			(emptyLattice ([], "B") ([], "T"))	-- filled later on
			funcs
			rels
			rules


-- | Converts the modules from parsetree into all the needed parts
_fullFileCombiner	:: Combiner 
				(Maybe (Grouper SyntacticForm),
				(Maybe (Grouper (Function' ())),
				(Maybe (Grouper Relation),
				Maybe (Grouper Rule)
				)))

_fullFileCombiner
	= let	s	= moduleCombiner "Syntax" syntaxDecl'
		f	= moduleCombiner "Functions" functionsCmb
		rels	= moduleCombiner "Relations" relations
		rules	= moduleCombiner "Rules" Relations.rules
		inJ cmb	= cmb |> Just 
		modules	= _optionalCombiners (inJ s) $
				_optionalCombiners (inJ f) $
				_optionalCombiners' (inJ rels)
				[inJ rules]
		modules'	= modules & reverse ||>> _chain ||>> (|> _chain)
		in
	  choices' "modules" modules'



_chain		:: (Maybe (Maybe a), Maybe (Maybe b, Maybe c)) -> (Maybe a, (Maybe b, Maybe c))
_chain (mma, mmbmc)
		= (join mma, distrEffect mmbmc)

_optionalCombiners'	:: Combiner (Maybe a) -> [Combiner (Maybe b)] -> [Combiner (Maybe a, Maybe b)]
_optionalCombiners' a b
	= _optionalCombiners a b ||>> (join *** join)

_optionalCombiners	:: Combiner a -> [Combiner b] -> [Combiner (Maybe a, Maybe b)]
_optionalCombiners cmbA cmbB
	= let	cmbA'	= cmbA |> Just
		cmbB'	= cmbB ||>> Just
		a	= cmbA' |> (\a -> (a, Nothing))
		as	= cmbB'
		fstNothing b	= (Nothing, b)
		in
		(a : (as ||>> fstNothing)) ++ [ cmbA' <+> a' | a' <- as ] 


_optionalOrder		:: (a -> a -> a) -> a -> [a] -> [a]
_optionalOrder plus a as
	= (a : as) ++ [ a `plus` a' | a' <- as]



{- | All the syntaxes needed to parse a language definition file

>>> let resolve		= Right	:: FQName -> Either String FQName
>>> let subtypeStub	= (==) :: FQName -> FQName -> Bool
>>> metaSyntaxes & M.toList |> (\(fq, s) -> check' (resolve, subtypeStub, fq) s) & allRight_ 
Right ()
-}
metaSyntaxes	:: Map [Name] Syntax
metaSyntaxes
	= let	syntax	= mainSyntax [("Syntax", (["Syntax"], "syntax"))
					, ("Functions", (["Functions"], "functions"))
					, ("Relations", (["Relations"], "relations"))
					, ("Rules", (["Relations"], "rules"))
					]
		syntaxes	=  [("Helper", helperSyntax)
					, ("ALGT", syntax)
					, ("Syntax", bnfSyntax)
					, ("Functions", functionSyntax)
					, ("Relations", relationSyntax)
					]
					|> first (:[]) & M.fromList
		in
		syntaxes 


		
saveMetaSyntaxes	:: IO ()
saveMetaSyntaxes
	= do	putStrLn "Saving metasyntaxes to the asset files..."
		dir	<- getCurrentDirectory |> (++"/src/Assets/MetaSyntax")
		putStrLn $ "Directory is: "++dir
		dirExists	<- doesDirectoryExist dir
		when dirExists $ removeDirectoryRecursive dir
		createDirectory dir
		metaSyntaxes & M.toList |+> uncurry (_saveMetaSyntax dir)
		pass


_saveMetaSyntax	:: FilePath -> [Name] -> Syntax -> IO ()
_saveMetaSyntax dir nm syntax
	= do	let target	= dir ++"/" ++ dots nm ++ ".language"
		print target
		let imports	= metaSyntaxes & M.keys |> dots |> ("import "++) & unlines
		let contents	= imports ++ 
				  inHeader " " (dots nm) '*' 
				  ("# Automatically generated; do not edit" ++ 
				  inHeader' "Syntax"
				  (toParsable syntax))
		writeFile target contents


-- Tests parsing of a rule against a given string
testSyntax	:: Name -> String -> IO ()
testSyntax rule string
	= do	let found	= metaSyntaxes & M.filter (\s -> rule `M.member` get grouperDict s) & M.keys
		unless (L.null found) $ error $ "No rule "++rule++" does exist within any syntax"
		when (length found > 1) $ error $ "Rule "++rule ++" does exist in "++(found |> dots & commas)
		let fqn	= found & head
		putStrLn $ showFQ (fqn, rule)
		let parsed	= parse "testSyntax" (metaSyntaxes, fqn) rule string
					 & either error id
		printPars parsed

------------------------------------- EXTERNAL Definitions ----------------------------------------


{- | The syntax that declares metafunctions, as defined in the Assets
>>> functionSyntax
Grouper {_grouperDict = fromList [("arguments",SyntacticForm ...
-}

functionSyntax	:: Syntax
functionSyntax
	= _loadAssetsSyntax "Functions" Assets._Functions_language


{- | The syntax that declares relations, as defined in the Assets
>>> relationSyntax
Grouper {_grouperDict = fromList [("commaSepExpr",SyntacticForm ...
-}
relationSyntax	:: Syntax
relationSyntax
	= _loadAssetsSyntax "Relations" Assets._Relations_language



_loadAssetsSyntax	:: Name -> String -> Syntax
_loadAssetsSyntax title contents
	= parseFullFile [title] ("Assets."++title++".language") contents
		& either error id
		& get langSyntax
		& fromMaybe (error $ title ++ " asset does not contain syntax?")
		& _patchFullQualifies [title]















------------------------------------------- Explicit BNF (of skeleton etc) -------------------------------------


choices' nm	= choices (["ALGT"], nm)


-- The syntax of the entire file skeleton, with: [("Syntax to call into", "Title", rule to call")
mainSyntax	:: [(Name, FQName)] -> Syntax
mainSyntax subRules
      =([ "stars        ::= \"*\" stars | \"*\""
	, "dashes       ::= \"-\" dashes | \"-\"" 
	, "eqs          ::= \"=\" eqs | \"=\""
	, "lineContents ::= LineChar lineContents"
	, "\t           | \"\\n\""
	, "comment      ::= \"#\" $lineContents"
	, "title        ::= imports $lineContents $stars Syntax.nls"
	, "\t           | $lineContents $stars Syntax.nls"
	, "namespace    ~~= IdentifierUpper \".\" namespace | IdentifierUpper"
	, "importStm    ::= \"import\" \"local\" | \"import\""
	, "import       ::= Syntax.nls importStm namespace | importStm namespace"
	, "imports      ::= import imports | Syntax.nls"
	]
	++ (subRules |> fst |> _title)
	++ (subRules |> _titledModCall |> snd)
	++ ["modules    ::= "++subRules |> _titledModCall |> fst & allOptional & intercalate "\n\t|"]
	++ [ "langDef      ::= title modules"
	]
	)  & unlines & asSyntaxUnchecked "ALGT" & either error id



_allOptional	:: [Name] -> [[String]]
_allOptional [name]	= [[name]]
_allOptional (name:names)
	= _optionalOrder (++) [name] $ _allOptional names

allOptional names
	= _allOptional names & reverse |> unwords

_moduleCall	:: (Name, FQName) -> String
_moduleCall (modName, calledRule)
	= [ "title"++modName
	  , BNF.RuleCall calledRule & toParsable
	  ] & unwords

_titledModCall	:: (Name, FQName) -> (Name, String)
_titledModCall info@(modName, _)
	= let	formName	= "module"++modName
		syntForm	= formName ++"    ::= "++ _moduleCall info ++ " Syntax.nls | "++ _moduleCall info in
		(formName, syntForm)


_title	:: Name -> String
_title nm
	= "title"++nm++"  \t::= "++show nm++" Syntax.nl eqs Syntax.nl"


_titleCombiner	:: Combiner (String, [String], [Import ()])
_titleCombiner
	= let base	= ((capture |> init) {-lineContents-} <+> (skip {-stars-} **> nls)) in
		choices' "title"	[cmb (\imps (nm, doc) -> (nm, doc, imps)) _imports base
				, base |> (\(nm, doc) -> (nm, doc, []))
				]


_imports	:: Combiner [Import ()]
_imports	= choices' "imports"
			[ cmb (:) _import _imports
			, skip' []
			]

_import		:: Combiner (Import ())
_import	= let base	= (_importStm <+> _nameSpace) & withLocation (,)
				|> (\(li, (local, ns)) doc -> Import ns local (MetaInfo li doc) ())
				:: Combiner (Doc -> Import ())
		in
		choices' "import"
			[ cmb (&) (nls |> concat) base
			, base |> (\f -> f "")
			]


_importStm	:: Combiner Bool
_importStm	= choices' "importStm"
			[lit "import" **> (lit "local" |> const True)
			, lit "import" |> const False
			]


_nameSpace	:: Combiner [Name]
_nameSpace	= choices' "namespace"
			[ cmb (:) capture (lit "." **> _nameSpace)
			, capture |> (:[])]

_modTitleCombiner	:: Name -> Combiner ()
_modTitleCombiner nm
	= choices' ("title"++nm) [skip **> skip **> skip **> skip]

moduleCombiner		:: Name -> Combiner la -> Combiner la
moduleCombiner title main
	= choices' ("module"++title)
		[ _modTitleCombiner title **> main <** skip
		, _modTitleCombiner title **> main]


parseLangDef		:: Combiner parts -> Combiner ((Name, [String], [Import ()]), parts)
parseLangDef parseModules
	= choices' "langDef"
		[_titleCombiner <+> parseModules]


instance Infoable (LanguageDef' a b) where
	getInfo ld
		= let	mi	= MetaInfo (get langLocation ld) (get langMeta ld & unlines)
			in
			AllInfo (get langTitle ld) "Language Definition" mi (toParsable ld)

instance ToString (LanguageDef' a b) where
	toParsable (LanguageDef title imports langMeta langLoc syntax _ functions rels rules)
		= let mayb header	= maybe "" (inHeader' header . toParsable) in
		  (imports |> toParsable & unlines) ++
		  inHeader "" title '*' (unlines (
			langMeta |> ("# "++)
			++ [ mayb "Syntax" syntax, mayb "Functions" functions, mayb "Relations" rels, mayb "Rules" rules]
			))


instance ToString (Import a) where
	toParsable (Import ns local meta _)
		= [ toParsable meta
		  , "import "++ (if local then "local " else "") ++ ns & intercalate "."] & unlines
