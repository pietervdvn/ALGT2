{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module LanguageDef.LanguageDef where

{-

A language defintion builds on many language-definition aspects. Each language definition aspect has its own section, for which it handles the parsing (and meta-data such as documentation)

 -}

import Utils.All

import qualified LanguageDef.Syntax.BNF as BNF
import LanguageDef.Syntax.All
import LanguageDef.MetaExpression
import LanguageDef.MetaFunction
import LanguageDef.LocationInfo


import Data.Char
import Data.List

import Data.Map as M
import Data.Either
import Data.Maybe
import Data.Bifunctor (first)

import qualified Assets


data Import a = Import
	{ _importName	:: [Name]
	, _isLocal	:: Bool
	, _importMeta	:: MetaInfo
	, _importA	:: a
	} deriving (Show, Eq, Functor)
makeLenses ''Import

-- The main definition of a language. Represents an entire langdef file
data LanguageDef' imported
	 = LanguageDef 
		{ _langTitle	:: Name		-- title of the language
		, _langImports	:: [Import imported]
		, _langMeta	:: [String]	-- The comments just under the title
		, _baseSyntax	:: Maybe Syntax	-- The syntax of the language, aka the BNF
		}
	deriving (Show, Eq)
makeLenses ''LanguageDef'


type ResolvedImport	= FilePath
type LanguageDef	= LanguageDef' ResolvedImport


resolveLocalImports	:: ([Name], [Name]) -> LanguageDef' x -> LanguageDef' x
resolveLocalImports (offsetForAll, extraOffsetForLocal)
	= let	fixImport imp	= if get isLocal imp 
					then imp & set isLocal False & over importName ((offsetForAll ++ extraOffsetForLocal) ++ )
					else imp & over importName (offsetForAll ++)
		in
		over (langImports . mapped) fixImport

fixImport		:: Map [Name] FilePath -> LanguageDef' () -> Either String LanguageDef
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




-- | Parses the entire file, file should still be checked against it's context!
-- >>> check' (metaSyntaxes, ["Main"]) (parseLangDef $ _fullFileCombiner ["Main"]) & either error id
-- ()
-- >>> parseFullFile ["TestLang"] "Test:Assets/TestLang" Assets._TestLanguage_language 
-- Right ...
parseFullFile	:: [Name] -> FilePath -> String -> Either String (LanguageDef' ())
parseFullFile ns fp contents
	= inMsg ("While parsing "++show fp) $
	  do	pt	<- parse fp (metaSyntaxes, ["Main"]) "langDef" contents
		((title, meta, imports), (syntax, _))	<- interpret (parseLangDef (_fullFileCombiner ns)) pt
		return $ LanguageDef title imports meta syntax



-- | Converts the modules from parsetree into all the needed parts
_fullFileCombiner	:: [Name] -> Combiner (Maybe Syntax, Maybe [Function LocationInfo])
_fullFileCombiner ns
	= let	s	= moduleCombiner "Syntax" syntaxDecl' |> Just
		f	= moduleCombiner "Functions" functions |> Just
		in
	  choices "modules" $ reverse
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
		& get baseSyntax
		& fromMaybe (error "Metafunctionsyntax asset does not contain syntax?")
		& _patchFullQualifies ["Functions"] -- TODO this line should become obsolete soon; when removed, remove the export of Syntax.all for _patchFullQualifies





















------------------------------------------- Explicit BNF (of skeleton etc) -------------------------------------





-- The syntax of the entire file skeleton, with: [("Syntax to call into", "Title", rule to call")
mainSyntax	:: [(Name, FQName)] -> Syntax
mainSyntax subRules
      =([ "stars        ::= \"*\" stars | \"*\""
	, "dashes       ::= \"-\" dashes | \"-\"" 
	, "eqs          ::= \"=\" eqs | \"=\""
	, "lineContents ::= LineChar lineContents"
	, "\t           | \"\\n\""
	, "comment      ::= \"#\" $lineContents"
	, "nl           ::= comment"
	, "\t           | \"\\n\""
	, "nls          ::= nl nls"
	, "\t           | nl"
	, "title        ::= imports $lineContents $stars nls"
	, "\t           | $lineContents $stars nls"
	, "namespace    ~~= IdentifierUpper \".\" namespace | IdentifierUpper"
	, "importStm    ::= \"import\" \"local\" | \"import\""
	, "import       ::= nls importStm namespace | importStm namespace"
	, "imports      ::= import imports | nls"
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
		syntForm	= formName ++"    ::= "++ _moduleCall info ++ " nls | "++ _moduleCall info in
		(formName, syntForm)


_title	:: Name -> String
_title nm
	= "title"++nm++"  \t::= "++show nm++" nl eqs nl"


_titleCombiner	:: Combiner (String, [String], [Import ()])
_titleCombiner
	= let base	= ((capture |> init) {-lineContents-} <+> (skip {-stars-} **> nls)) in
		choices "title"	[cmb (\imps (nm, doc) -> (nm, doc, imps)) _imports base
				, base |> (\(nm, doc) -> (nm, doc, []))
				]

_imports	:: Combiner [Import ()]
_imports	= choices "imports"
			[ cmb (:) _import _imports
			, skip' []
			]

_import		:: Combiner (Import ())
_import	= let base	= (_importStm <+> _nameSpace) & withLocation (,)
				|> (\(li, (local, ns)) doc -> Import ns local (MetaInfo li doc) ())
				:: Combiner (Doc -> Import ())
		in
		choices "import"
			[ cmb (&) (nls |> concat) base
			, base |> (\f -> f "")
			]


_importStm	:: Combiner Bool
_importStm	= choices "importStm"
			[lit "import" **> (lit "local" |> const True)
			, lit "import" |> const False
			]


_nameSpace	:: Combiner [Name]
_nameSpace	= choices "namespace"
			[ cmb (:) capture (lit "." **> _nameSpace)
			, capture |> (:[])]

_modTitleCombiner	:: Name -> Combiner ()
_modTitleCombiner nm
	= choices ("title"++nm) [skip **> skip **> skip **> skip]

moduleCombiner		:: Name -> Combiner la -> Combiner la
moduleCombiner title main
	= choices ("module"++title)
		[ _modTitleCombiner title **> main <** skip
		, _modTitleCombiner title **> main]


parseLangDef		:: Combiner parts -> Combiner ((Name, [String], [Import ()]), parts)
parseLangDef parseModules
	= choices "langDef"
		[_titleCombiner <+> parseModules]



instance ToString (LanguageDef' a) where
	toParsable (LanguageDef title imports langMeta syntax)
		= let mayb header stuff	= maybe "" (\stuff' -> inHeader' header $ toParsable stuff') stuff in
		  (imports |> toParsable & unlines) ++
		  (inHeader "" title '*' $ unlines (
			langMeta |> ("# "++)
			++ [ mayb "Syntax" syntax]
			))


instance ToString (Import a) where
	toParsable (Import ns local meta _)
		= [ toParsable meta
		  , "import "++ (if local then "local " else "") ++ ns & intercalate "."] & unlines
