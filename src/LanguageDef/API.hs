module LanguageDef.API 
		(loadLangDef, loadAssetLangDef
		, createParseTree, createExpression, createTypedExpression, parseTarget, createPredicate
		, LDScope.resolveGlobal, LDScope.resolve, LDScope.resolve', LDScope.allKnowns
		, LDScope.syntaxCall, LDScope.functionCall, LDScope.ruleCall, LDScope.relationCall
		, LDScope.LDScope, FQName, showFQ
		, resolveGlobal'
		, infoAbout, infoAbout', inScope, infoImports
		, testLanguage, stfl
		, runFunction, runExpression, runExpression', runPredicate, runPredicate'
		, typeTop, typeBottom, supertypes
		) where

{- 

Creates all kinds of stuff based on strings, to test with, such as expressions of functions, parsetrees, ...

This should all be based on some syntax

 -}

import Utils.All
import Utils.PureIO (PureIO, runIO, runPure)

import AssetUtils

import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.LocationInfo

import qualified LanguageDef.Combiner as Combiner

import LanguageDef.Data.LanguageDef
import LanguageDef.Data.Expression
import LanguageDef.Data.ParseTree
import LanguageDef.Data.Function
import LanguageDef.Data.SyntFormIndex
import LanguageDef.Data.Rule
import LanguageDef.Data.Relation (predicate)
import LanguageDef.Data.Proof
import LanguageDef.MetaSyntax (typeTop, typeBottom)



import LanguageDef.Interpreter (resolveAndRun, evalExpression)
import LanguageDef.Typer
import LanguageDef.LangDefs as LDScope
import LanguageDef.LangDefsFix as LDF
import LanguageDef.ModuleLoader
import LanguageDef.Prover

import Data.Maybe

import Data.Map (Map, (!), empty)


import Graphs.Lattice (Lattice)

{- Loads a language definition from the filesystem; use with 'runIO'-}
loadLangDef	:: FilePath -> [Name] -> PureIO (Failable LDScope)
loadLangDef fp plzLoad
		= loadAll fp plzLoad |> fst		


{- Loads a language definition directly from the assets; is Pure -}
loadAssetLangDef	:: FilePath -> [Name] -> Failable LDScope
loadAssetLangDef fp names
		= runPure allAssets' (loadLangDef fp names) |> fst & either fail id


testLanguage 	= loadAssetLangDef "" ["TestLanguage"] & crash'

stfl		= loadAssetLangDef "TestLanguages" ["STFL"] & crash'

{- | Gives info about any entry, resolved globally
>>> infoAbout testLanguage ["TestLanguage", "bool"] |> uncurry toParsable' & unlines
"\n TestLanguage.bool (Syntactic Form) \n====================================\n\n\n\nbool\t::= \"True\"\t\n\t  | \"False\"\t\n\n"


>>> infoAbout testLanguage ["TestLanguage", "and"] |> uncurry toParsable' & unlines
"\n TestLanguage.and (Function) \n=============================\n\n\nand\t : TestLanguage.bool \215 TestLanguage.bool \8594 TestLanguage.bool\nand(\"True\", \"True\")\t = \"True\"\nand(_, _)\t = \"False\"\n\n\n\n TestLanguage.and (Rule) \n=========================\n\n\n \n---------------------------------------------- [ and ]\n (TestLanguage.\8594) (\"True\" \"&\" \"True\"), \"True\"\n\n\n"
>>> infoAbout testLanguage ["TestLanguage"]|> uncurry toParsable' & unlines
"\n TestLanguage. (Language Definition) \n=====================================\n\n\n Test Language \n***************\n\n# Blabla\n\n\n Syntax \n========\n\n\n\nbool\t::= \"True\"\t\n\t  | \"False\"\t\n\n\nint\t::= Number\t\n\n\nexpr\t::= TestLanguage.bool\t\n\t  | TestLanguage.int\t\n\n\nexprSum\t::= TestLanguage.expr TestLanguage.op TestLanguage.exprSum\t\n\t  | TestLanguage.expr\t\n\n\nop\t::= \"&\"\t\n\t  | \"+\"\t\n\n\ntuple\t::= TestLanguage.expr TestLanguage.expr\t\n\n\n\n Functions \n===========\n\n# Inverts a boolean\n# \nnot\t : TestLanguage.bool \8594 TestLanguage.bool\nnot(\"True\")\t = \"False\"\nnot(\"False\")\t = \"True\"\n\n\nand\t : TestLanguage.bool \215 TestLanguage.bool \8594 TestLanguage.bool\nand(\"True\", \"True\")\t = \"True\"\nand(_, _)\t = \"False\"\n\n\nnand\t : TestLanguage.bool \215 TestLanguage.bool \8594 TestLanguage.bool\nnand(a, b)\t = TestLanguage.not(TestLanguage.and(a, b))\n\n\nor\t : TestLanguage.bool \215 TestLanguage.bool \8594 TestLanguage.bool\nor(TestLanguage.not(\"True\"), TestLanguage.not(\"True\"))\t = \"False\"\nor(_, _)\t = \"True\"\n\n\n\n\n Relations \n===========\n\n\n(\8594)\tTestLanguage.exprSum (in) \215 TestLanguage.exprSum (out); Pronounced as \"smallstep\"\n\n\n\n\n Rules \n=======\n\n\n \n---------------------------------------------- [ and ]\n (TestLanguage.\8594) (\"True\" \"&\" \"True\"), \"True\"\n\n\n\n\n\n\n"

>>> infoAbout testLanguage ["~"]
[]


>>> infoAbout testLanguage ["TestLanguage", "→"] |> uncurry toParsable' & unlines
"\n TestLanguage.\8594 (Relation) \n===========================\n\n\n(\8594)\tTestLanguage.exprSum (in) \215 TestLanguage.exprSum (out); Pronounced as \"smallstep\"\n\n\n"

-}
infoAbout	:: LDScope -> [Name] -> [(FQName, AllInfo)]
infoAbout lds names
	= let 	fqn	= (init names, last names)
		-- the module to eventually give info over
		ldInfo	= lds & enterScope names & fromFailable 
					|> get ldScope |> getInfo |> (,) (names, "") & maybeToList

		-- The language def to actually work in
		otherInfo'	= do	inPhase Resolving $ assert' (length names > 1)
						"To get information about entries which are globally resolved, a scope qualifier is needed" 
					ld		<- lds & enterScope (init names)
					return $ infoAbout' ld names
		otherInfo	= otherInfo' & fromFailable & fromMaybe []
		in ldInfo ++ otherInfo

{- | Gives info about any entry, resolved from within the module. An entity could be not in scope or could be info about an import
>>> infoAbout' testLanguage ["TestLanguage", "bool"] |> uncurry toParsable' & unlines
"\n TestLanguage.bool (Syntactic Form) \n====================================\n\n\n\nbool\t::= \"True\"\t\n\t  | \"False\"\t\n\n"
>>> infoAbout' testLanguage ["bool"] |> uncurry toParsable' & unlines
"\n TestLanguage.bool (Syntactic Form) \n====================================\n\n\n\nbool\t::= \"True\"\t\n\t  | \"False\"\t\n\n"
>>> infoAbout' testLanguage ["and"] |> uncurry toParsable' & unlines
"\n TestLanguage.and (Function) \n=============================\n\n\nand\t : TestLanguage.bool \215 TestLanguage.bool \8594 TestLanguage.bool\nand(\"True\", \"True\")\t = \"True\"\nand(_, _)\t = \"False\"\n\n\n\n TestLanguage.and (Rule) \n=========================\n\n\n \n---------------------------------------------- [ and ]\n (TestLanguage.\8594) (\"True\" \"&\" \"True\"), \"True\"\n\n\n"
-}
infoAbout'	:: LDScope -> [Name] -> [(FQName, AllInfo)]
infoAbout' lds names
	= let	fqn		= (init names, last names)
		search x	= resolve' lds x fqn & fromFailable ||>> getInfo
		in
		catMaybes [search syntaxCall, search functionCall, search relationCall, search ruleCall]




{- | Dumps all info about all entries in scope
>>> inScope testLanguage |> fst |> fst |> showFQ & unlines
"\8868\n\8869\nTestLanguage.bool\nTestLanguage.expr\nTestLanguage.exprSum\nTestLanguage.int\nTestLanguage.op\nTestLanguage.tuple\nTestLanguage.and\nTestLanguage.nand\nTestLanguage.not\nTestLanguage.or\nTestLanguage.\8594\nTestLanguage.and\n"

-}	
inScope		:: LDScope -> [((FQName, [FQName]), AllInfo)]
inScope scope
	= let	ak x	= allKnowns scope x |> (\((fqn, _), names, x) -> ((fqn, names), getInfo x) )
		in	
		ak syntaxCall ++ ak functionCall ++ ak relationCall ++ ak ruleCall

{- | Gives the import flags for a scope
>>> infoImports testLanguage
fromList [(["TestLanguage"],ImportFlags {_ifOrigin = "/TestLanguage.language", _ifIsSelf = True, _ifDiffName = [["TestLanguage"],[]]})]

-}
infoImports	:: LDScope -> Map [Name] ImportFlags
infoImports scope
	= scope & get imported


supertypes	:: LDScope -> Lattice FQName
supertypes lds
	= lds & get (ldScope . langSupertypes)

resolveGlobal' lds entity fqn
	= resolveGlobal lds entity fqn |> snd



runFunction  	:: LDScope -> FQName -> [ParseTree] -> Failable ParseTree
runFunction	= resolveAndRun


runExpression	:: LDScope -> Expression -> Failable ParseTree
runExpression 	= evalExpression

runExpression'	:: LDScope -> FilePath -> FQName -> String -> Failable ParseTree
runExpression' lds file expectedType input
	= do	expr	<- createTypedExpression lds file input expectedType
		runExpression lds expr



runPredicate	:: LDScope -> Predicate -> Failable Proof
runPredicate	= proofThat



{- >>> runPredicate' testLanguage "?" "not(\"True\")" & crash
ProofExpr {_exprResult = Literal {_ptToken = "False", _ptLocation = LocationInfo {_liStartLine = -1, _liEndLine = -1, _liStartColumn = -1, _liEndColumn = -1, _miFile = ""}, _ptA = (), _ptHidden = False}}
>>> runPredicate' testLanguage "?" "(→) \"True\" \"&\" \"True\", x" & crash
"------------------------------- [and]\n(TestLanguage.\8594)True&True, True\n"

-}
runPredicate'	:: LDScope -> FilePath -> String -> Failable Proof
runPredicate' lds fp input
	= do	pred	<- createPredicate lds fp input
		runPredicate lds pred


{- | createParseTree: creates a parsetree based on the syntax of LDScope
>>> createParseTree testLanguage (["TestLanguage"], "exprSum") "?" "True & True" |>  toParsable
Success "True & True"
 -}
createParseTree	:: LDScope -> FQName -> FilePath -> String -> Failable ParseTree
createParseTree	= parseTarget

{- | Creates an entire expression, which is untyped 
>>> createExpression testLanguage "?" "and(\"True\", \"True\")" |> toParsable
Success "and(\"True\", \"True\")"
-}
createExpression	:: LDScope -> FilePath -> String -> Failable (Expression' ())
createExpression ld source str
	= do	pt	<- parse source (metaSyntaxes, ["Functions"]) "expression" str
		Combiner.interpret expression pt

{- | Creates an expression and types it 
>>> createTypedExpression testLanguage "?" "and(\"True\",\"True\")" (["TestLanguage"],"bool") |> toParsable
Success "TestLanguage.and(\"True\", \"True\")"

-}

createTypedExpression	:: LDScope -> FilePath -> String -> FQName -> Failable Expression
createTypedExpression ld source str typ@(loc, nm)
	= do	expr	<- createExpression ld source str
		typeExpression ld typ expr ||>> snd



createPredicate		:: LDScope -> FilePath -> String -> Failable Predicate
createPredicate lds source str
	= do	pt	<- parse source (metaSyntaxes, ["Relations"]) "predicate" str
		pred	<- Combiner.interpret predicate pt
		pred'	<- typePredicate lds pred	:: Failable (Predicate' ((), SyntFormIndex))
		pred' |> snd & return


