{-# LANGUAGE TemplateHaskell #-}
module LanguageDef.LanguageDef where

{-

A language defintion builds on many language-definition aspects. Each language definition aspect has its own section, for which it handles the parsing (and meta-data such as documentation)

 -}

import Utils.All

import LanguageDef.Syntax.All
import LanguageDef.MetaExpression
import LanguageDef.MetaFunction
import LanguageDef.LocationInfo


import Data.Char
import Data.List

import Data.Map as M
import Data.Either
import Data.Maybe

import Test.DocTest

import qualified Assets

-- The main definition of a language. Represents an entire langdef file
data LanguageDef = LanguageDef 
		{ _langTitle	:: Name		-- title of the language
		, _langMeta	:: [String]	-- The comments just under the title
		, _baseSyntax	:: Maybe Syntax	-- The syntax of the language, aka the BNF
		}
	deriving (Show, Eq)

makeLenses ''LanguageDef



-- Parses the entire file
parseFullFile	:: FilePath -> String -> Either String LanguageDef
parseFullFile fp contents
	= do	let syntaxes	= metaSyntaxes
		pt	<- parse fp (syntaxes, "main") "langDef" contents
		((title, meta), (syntax, _))	<- interpret (parseLangDef _fullFileCombiner) pt
		syntax |> asSyntaxes' |> check & fromMaybe pass
		return $ LanguageDef title meta syntax



-- | Converts the modules from parsetree into all the needed parts
-- >>> check' (metaSyntaxes, "main") (parseLangDef _fullFileCombiner)
-- Right ()
_fullFileCombiner	:: Combiner (Maybe Syntax, Maybe [Function LocationInfo])
_fullFileCombiner
	= let	s	= moduleCombiner "Syntax" syntaxDecl' |> Just
		f	= moduleCombiner "Functions" functions |> Just
		in
	  choices "modules" $ reverse
			[ s |> (\s -> (s, Nothing))
			, f |> (\f -> (Nothing, f))
			, s <+> f] 

-- >>> check metaSyntaxes
-- Right ()
metaSyntaxes	:: Map Name Syntax
metaSyntaxes
	= let	syntax	= mainSyntax [("Syntax", "syntax"), ("Functions", "functions")]
		syntaxes	= M.fromList [("helper", helperSyntax), ("main", syntax), ("syntax", bnfSyntax), ("functions", functionSyntax)]
		in
		syntaxes 


		
debugMetaSynt :: String -> IO ()
debugMetaSynt str
	= do	synt	<- checkExistsSugg id str metaSyntaxes ("Metasyntax "++str++" not found")
				 & either error return
		printPars synt
		


------------------------------------- EXTERNAL Definitions ----------------------------------------

functionSyntax	:: Syntax
functionSyntax
	= parseFullFile "Assets.MetaFunctionSyntax.language" Assets._MetaFunctionSyntax_language 
		& either error id
		& get baseSyntax
		& fromMaybe (error "Metafunctionsyntax asset does not contain syntax?")





















------------------------------------------- Explicit BNF (of skeleton etc) -------------------------------------





-- The syntax of the entire file skeleton
mainSyntax	:: [(Name, Name)] -> Syntax
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
	, "title        ::= nls $lineContents $stars nls"
	, "\t           | $lineContents $stars nls"
	]
	++ (subRules |> fst |> _title)
	++ (subRules |> _titledModCall |> snd)
	++ ["modules    ::= "++subRules |> _titledModCall |> fst & allOptional & intercalate "\n\t|"]
	++ [ "langDef      ::= title modules"
	]
	)  & unlines & asSyntaxUnchecked "Language Def syntax construction" & either error id



allOptional	:: [Name] -> [String]
allOptional names
	= subsequences names & tail & reverse
		|> unwords

_moduleCall	:: (Name, Name) -> String
_moduleCall (modName, calledRule)
	= [ "title"++modName
	  , (modName |> toLower)++"."++calledRule
	  ] & unwords

_titledModCall	:: (Name, Name) -> (Name, String)
_titledModCall info@(modName, calledRule)
	= let	formName	= "module"++modName
		syntForm	= formName ++"    ::= "++ _moduleCall info ++ " nls | "++ _moduleCall info in
		(formName, syntForm)


_title	:: Name -> String
_title nm
	= "title"++nm++"  \t::= "++show nm++" nl eqs nl"

_module	:: (Name, Name) -> String
_module (ns, rule)
	= (ns |> toLower) ++"."++rule

_titleCombiner	:: Combiner (String, [String])
_titleCombiner
	= let base	= ((capture |> init) {-lineContents-} <+> (skip {-stars-} **> nls)) in
		choices "title"	[ skip {-nls-} **> base, base]

_modTitleCombiner	:: Name -> Combiner ()
_modTitleCombiner nm
	= choices ("title"++nm) [skip **> skip **> skip **> skip]

moduleCombiner		:: Name -> Combiner la -> Combiner la
moduleCombiner title main
	= choices ("module"++title)
		[ _modTitleCombiner title **> main <** skip
		, _modTitleCombiner title **> main]


parseLangDef		:: Combiner parts -> Combiner ((Name, [String]), parts)
parseLangDef parseModules
	= choices "langDef"
		[_titleCombiner <+> parseModules]



instance ToString LanguageDef where
	toParsable (LanguageDef title langMeta syntax)
		= let mayb header stuff	= maybe "" (\stuff' -> inHeader' header $ toParsable stuff') stuff in
		  inHeader "" title '*' $ unlines (
			langMeta |> ("# "++)
			++ [ mayb "Syntax" syntax]
			)


