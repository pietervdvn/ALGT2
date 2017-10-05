{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, DeriveFunctor #-}
module LanguageDef.Data.Relation where


import Utils.All
import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Utils.Checkable
import LanguageDef.Utils.Grouper

import LanguageDef.Data.Rule
import LanguageDef.Data.Expression hiding (choices')

import LanguageDef.Combiner
import LanguageDef.MetaSyntax (nl, nls)

import Data.Map (Map, (!), filterWithKey)
import qualified Data.Map as M

import Control.Arrow ((&&&))





data Mode	= In | Out
		deriving (Show, Eq)

data Relation	= Relation
		{ _relSymbol	:: Name
		, _relTypes	:: [(FQName, Mode)]
		, _relPronounce	:: Maybe String
		, _relDocs	:: MetaInfo
		} deriving (Show, Eq)
makeLenses ''Relation







---------------------------- PARSING -----------------------

choices' nm	= choices (["Relations"], nm)


relIdent	:: Combiner FQName
relIdent	= choices' "relIdent"
			[ cmb (\head (tail, nm) -> (head:tail, nm))
				capture (lit "." **> relIdent)
			, capture |> (,) []
			]

mode		= choices' "mode"
			[ lit "in" |> const In
			, lit "out" |> const Out
			]

typ :: Combiner (FQName, Mode)
typ		= choices' "type"
			[ ident <+> lit "(" **> mode <** lit ")"
			]



types		= choices' "types"
			[ cmb (:) typ (skip **> types)
			, typ |> (:[])
			]

relDeclarationCore :: Combiner (Name, [(FQName, Mode)])
relDeclarationCore
		= choices' "relDeclarationCore"
			[ lit "(" **> capture <+> lit ")" **> lit ":" **> types
			]


relPronounc	= choices' "relPronounce"
			[ lit ";" **> lit "Pronounced" **> lit "as" **> (capture |> Just) <+> nl
			, nl |> (,) Nothing
			]


relation	:: Combiner Relation
relation	= choices' "relation"
			[ (nls <+> relDeclarationCore <+> relPronounc) & withLocation _asRel
			, (relDeclarationCore <+> relPronounc) |> (,) [] & withLocation _asRel
			]

_asRel		:: LocationInfo 
			-> ([String], ((Name, [(FQName, Mode)]), (Maybe String, Maybe String)))
			-> Relation
_asRel li (docs, ((symbol, types), (pronounce, extraComment)))
	= let	docs'	= unlines docs ++ fromMaybe "" extraComment in
		Relation symbol types pronounce $ MetaInfo li docs'


_relations	:: Combiner [Relation]
_relations	= choices' "relations"
			[cmb (:) relation _relations
			, relation |> (:[]) 
			]


relations	= _relations |> asGrouper ("relation", "relations") (get relSymbol)

commaSepExpr	:: Combiner [Expression' ()]
commaSepExpr
	= choices' "commaSepExpr"
		[ cmb (:) expression (lit "," **> commaSepExpr)
		, expression |> (:[])
		]


conclusion	:: Combiner (Conclusion' ())
conclusion
	= choices' "conclusion"
		[ (lit "(" **> relIdent <+> lit ")" **> commaSepExpr)
			|> uncurry Conclusion
		]


predicate	:: Combiner (Predicate' ())
predicate
	= choices' "predicate"
		[ conclusion & withLocation' PredConcl
		, expression & withLocation' PredExpr
		]

predicates	:: Combiner [Predicate' ()]
predicates
	= choices' "predicates"
		[ cmb (:) predicate (lit "\t" **> predicates)
		, predicate |> (:[])
		]


line	:: Combiner String
line	= choices' "line" 
		[ skip **> lit "[" **> capture <** lit "]"
		, skip |> const ""
		]

rule	= choices' "rule"
		[ (nls <+> predicates <+> skip **> 
			line <+> skip **> 
			conclusion <** skip) 
			& withLocation _asRule
		, (nls <+> 
			line <+> skip **> 
			conclusion <** skip) 
			& withLocation _asRule'
		]

_asRule'	:: LocationInfo -> ([String], (Name, Conclusion' a)) -> Rule' a
_asRule' li (docs, (name, concl))
	= _asRule li (docs, ([], (name, concl)))

_asRule	:: LocationInfo -> ([String], ([Predicate' a], (Name, Conclusion' a))) -> Rule' a
_asRule li (docs, (preds, (name, conclusion)))
	= Rule preds conclusion name $ MetaInfo li (unlines docs)

_rules	:: Combiner [Rule' ()]
_rules	= choices' "rules"
		[ cmb (:) rule _rules
		, rule |> (:[])
		]


rules	= _rules |||>>> const () |> asGrouper ("rule", "rules") (get ruleName)


------------------------------------- CHECKS --------------------------------

{- | Checks the relation. Duplicate relation check is done by the grouper; unknown type check is done by the qualfier

>>> import LanguageDef.API
>>> loadAssetLangDef "Faulty/Relations" ["UnknownTypeRelation"] & toCoParsable
"| While fully qualifiying the relation form \"~\" in Faulty/Relations/UnknownTypeRelation.language at lines 11 - 13\n  Error: \n    \8226 The syntactic form \"x\" was not found within the namespace \n    \8226 Perhaps you meant: a, UnknownTypeRelation.a\n  Error: \n    \8226 The syntactic form \"x\" was not found within the namespace \n    \8226 Perhaps you meant: a, UnknownTypeRelation.a"
>>> loadAssetLangDef "Faulty/Relations" ["DuplicateRelation"] & toCoParsable
"| While validating the relation declarations while validating \nError: \n  \8226 The relation \"~\" is defined multiple times"
>>> loadAssetLangDef "Faulty/Relations" ["AllOutRel"] & toCoParsable
"| While validating the relation declarations while validating \nError: \n  \8226 Relation (~) should have at least one input type"
-}
instance Checkable Relation where
	check relation
		= do	let modes	= relation & get relTypes |> snd
			assert' (In `elem` modes) $ "Relation "++inParens (get relSymbol relation) ++" should have at least one input type"


{- |
>>> import LanguageDef.API
>>> loadAssetLangDef "Faulty/Relations" ["EmptyLine"] & toCoParsable
"| While validating the relation implementation while validating \nError: \n  \8226 This rule has no name. Add a name after the line, in square brackets\n  \n   predicate\n  ----------- [ name ]\n   (~) args"
>>> loadAssetLangDef "Faulty/Relations" ["NotDeclared"] & toCoParsable
"| While validating \nError: \n  \8226 When rules are defined, a relation declaration section should be present"
>>> loadAssetLangDef "Faulty/Relations" ["NotLocal"] & toCoParsable
"| While fully qualifiying the rule \"abc\" in Faulty/Relations/NotLocal.language at lines 18 - 22\n| While fully qualifiying a conclusion using ([],\"~\") in Faulty/Relations/NotLocal.language at line 21, columns 4 - 5\nError: \n  \8226 The relation \"~\" was not found within the namespace \n  \8226 Perhaps you meant: \8594, NotLocal.\8594"
-}
instance Checkable' (Grouper Relation) (Rule' a) where
	check' relations rule
		= do	assert' (not $ null $ get ruleName rule) 
				"This rule has no name. Add a name after the line, in square brackets\n\n predicate\n----------- [ name ]\n (~) args"
			let ruleAbout	= rule & get (ruleConcl . conclRelName) & snd
			assert' (ruleAbout `M.member` get grouperDict relations)
				$ ["Rule", show $ get ruleName rule, "is about relation", inParens ruleAbout, "which is not declared in this document. Only locally declared relations can be implemented with rules"]  & unwords


------------------------------------- TOPARSABLE -----------------------------

instance ToString Relation where
	toParsable (Relation symb tps pronounce doc)
	      = [ toParsable doc
		, "(" ++ symb ++ ")\t"  ++ 
			(tps |> _typeModeToPars & intercalate " × ") ++
			maybe "" ("; Pronounced as " ++ ) pronounce
		] & unlines



_typeModeToPars	:: (FQName, Mode) -> String
_typeModeToPars	(typ, mode)
	= [showFQ typ, inParens $ toParsable mode] & unwords

instance ToString Mode where
	toParsable In	= "in"
	toParsable Out	= "out"



instance Infoable Relation where
	getInfo r	= AllInfo (get relSymbol r) "Relation" (get relDocs r) (toParsable r)


