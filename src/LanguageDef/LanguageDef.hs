{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module LanguageDef.LanguageDef where

{-

A language defintion builds on many language-definition aspects. Each language definition aspect has its own section, for which it handles the parsing (and meta-data such as documentation)

 -}

import Utils.All

import qualified LanguageDef.Syntax.BNF as BNF
import LanguageDef.Syntax.All
import LanguageDef.MetaExpression hiding (choices')
import LanguageDef.MetaFunction hiding (choices')
import LanguageDef.LocationInfo

import Graphs.Lattice

import Data.Char
import Data.List

import Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Maybe
import Data.Bifunctor (first)
import Control.Arrow ((&&&))

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
		, _langSyntax		:: Maybe Syntax	-- The syntax of the language, aka the BNF
		, _langSupertypes	:: Lattice FQName	-- The global supertype relationship; is filled in later on by the langdefs-fixes
		, _langFunctions	:: Maybe (Functions' funcResolution)
		}
	deriving (Show, Eq)
makeLenses ''LanguageDef'


type ResolvedImport	= FilePath
type LanguageDef	= LanguageDef' ResolvedImport SyntFormIndex



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



-- | Parses the entire file, file should still be checked against it's context!
-- >>> check' metaSyntaxes (parseLangDef $ _fullFileCombiner ["Main"]) & either error id
-- ()
-- >>> parseFullFile ["TestLang"] "Test:Assets/TestLang" Assets._TestLanguage_language 
-- Right ...
parseFullFile	:: [Name] -> FilePath -> String -> Either String (LanguageDef' () ())
parseFullFile ns fp contents
	= inMsg ("While parsing "++show fp) $
	  do	pt	<- parse fp (metaSyntaxes, ["Main"]) "langDef" contents
		((title, meta, imports), (syntax, funcs))	<- interpret (parseLangDef (_fullFileCombiner ns)) pt
		return $ LanguageDef
			title
			imports
			meta
			syntax
			(emptyLattice ([], "B") ([], "T"))	-- filled later on
			(funcs |> _prepFunctions)


_prepFunctions	:: [Function LocationInfo] -> Functions' ()
_prepFunctions funcs
	= let	funcOrder	= funcs |> get funcName
		funcDict	= funcs |> (get funcName &&& id)
					& M.fromList
					||>> const ()
		in
		Functions funcDict funcOrder


-- | Converts the modules from parsetree into all the needed parts
_fullFileCombiner	:: [Name] -> Combiner (Maybe Syntax, Maybe [Function LocationInfo])
_fullFileCombiner ns
	= let	s	= moduleCombiner "Syntax" syntaxDecl' |> Just
		f	= moduleCombiner "Functions" functionsCmb |> Just
		in
	  choices' "modules" $ reverse
			[ s |> (\s -> (s, Nothing))
			, f |> (\f -> (Nothing, f))
			, s <+> f] 

-- | All the syntaxes needed to parse a language definition file
-- >>> check metaSyntaxes
-- Right ()
metaSyntaxes	:: Map [Name] Syntax
metaSyntaxes
	= let	syntax	= mainSyntax [("Syntax", (["Syntax"], "syntax"))
					, ("Functions", (["Functions"], "functions"))]
		syntaxes	=  [("Helper", helperSyntax), ("Main", syntax), ("Syntax", bnfSyntax), ("Functions", functionSyntax)]
					|> first (:[]) & M.fromList
		in
		syntaxes 


		


------------------------------------- EXTERNAL Definitions ----------------------------------------


{- | The syntax that declares metafunctions, as defined in the Assets
>>> functionSyntax
Syntax ...
-}

functionSyntax	:: Syntax
functionSyntax
	= parseFullFile ["Functions"] "Assets.MetaFunctionSyntax.language" Assets._MetaFunctionSyntax_language 
		& either error id
		& get langSyntax
		& fromMaybe (error "Metafunctionsyntax asset does not contain syntax?")
		& _patchFullQualifies ["Functions"] -- TODO this line should become obsolete soon; when removed, remove the export of Syntax.all for _patchFullQualifies





















------------------------------------------- Explicit BNF (of skeleton etc) -------------------------------------


choices' nm	= choices (["Main"], nm)


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
	)  & unlines & asSyntaxUnchecked "Main" & either error id



allOptional	:: [Name] -> [String]
allOptional names
	= subsequences names & tail & reverse
		|> unwords

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



instance ToString (LanguageDef' a b) where
	toParsable (LanguageDef title imports langMeta syntax _ functions)
		= let mayb header	= maybe "" (inHeader' header . toParsable) in
		  (imports |> toParsable & unlines) ++
		  inHeader "" title '*' (unlines (
			langMeta |> ("# "++)
			++ [ mayb "Syntax" syntax, mayb "Functions" functions]
			))


instance ToString (Import a) where
	toParsable (Import ns local meta _)
		= [ toParsable meta
		  , "import "++ (if local then "local " else "") ++ ns & intercalate "."] & unlines
