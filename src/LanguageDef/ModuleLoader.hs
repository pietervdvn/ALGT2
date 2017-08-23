{-# LANGUAGE TemplateHaskell #-}
module LanguageDef.ModuleLoader where

{- Responsible for finding and loading the appropriate language files -}

import Prelude hiding (putStr, putStrLn, readFile, putStrLn, fail)

import Utils.All
import Utils.PureIO

import LanguageDef.LanguageDef
import LanguageDef.LangDefs
import LanguageDef.LangDefsFix
import LanguageDef.Scope

import Data.Map as M

import Control.Monad hiding (fail)







data LoadingStatus = LS
	{ _rootModule	:: [Name]
	, _rootPath	:: FilePath
	, _currentlyKnown	:: Map [Name] (FilePath, LanguageDef' () ())
	}
makeLenses ''LoadingStatus



{- | Load all modules, given the root path where to start looking

>>> import AssetUtils


>>> lds = runPure allAssets' (loadAll "" ["TestInput","Nested","L"]) |> fst
>>> lds
Right (LangDefs ...)
>>> Right (LangDefs langs) = lds
>>> langL = langs ! ["TestInput", "Nested", "L"]
>>> (resolve langL syntaxCall) ([], "a")
Right (["TestInput","Nested","L"],"a")
>>> (resolve langL syntaxCall) ([], "x")
Right (["TestInput","Nested","X"],"x")

>>> runPure allAssets' (loadAll "TestInput" ["LoopingSupertypes"]) |> fst
Left "Cycles are detected in the supertype ..."


-}


loadAll	:: FilePath -> [Name] -> PureIO LangDefs
loadAll fp plzLoad
	= do	defs	<- _loadAll (LS [] fp M.empty) plzLoad |> get currentlyKnown |> _fixImports
		asLangDefs defs & either fail return
		

-- Adds the filepath to the imports, within the language def data
_fixImports	:: Map [Name] (FilePath, LanguageDef' () fr) -> Map [Name] (LanguageDef' ResolvedImport fr)
_fixImports langDefs
	= let	resolver	= langDefs |> fst in
		langDefs |> snd |> fixImport resolver |> either error id



_loadAll	:: LoadingStatus -> [Name] -> PureIO LoadingStatus
_loadAll ls toLoad
	= do	let path	= ((get rootPath ls : toLoad) & intercalate "/") ++ ".language"
		let msgs	= [ "Target", intercalate "." toLoad
				  , "Root filepath", get rootPath ls
				  , "Relative namespace", intercalate "." (get rootModule ls)
				  , "Target path", path
				  ] & perTwo (\a b -> "  "++a ++ ": "++b) & intercalate "\n"
		let msg	= "\nLoading:\n"++msgs
		putStr msg
		contents	<- readFile path
		-- Language def with unresolved inputs
		let absName	= get rootModule ls ++ toLoad
		ld	<- parseFullFile absName path contents & either fail return
		check ld & either fail return
		let ld'	= ld & resolveLocalImports (get rootModule ls, init toLoad)
		let ls'	= ls & over currentlyKnown (M.insert absName (path, ld'))
		
		putStrLn (ld & get langImports |> toParsable & intercalate "\n" & indent)

		ld 	& get langImports
			& foldM (_loadImport toLoad) ls'



-- Loads the imports recursively
_loadImport	:: [Name] -> LoadingStatus -> Import x -> PureIO LoadingStatus
_loadImport newRootModule ls imprt
 | get isLocal imprt	
	= do	let pathDiff	= init newRootModule
		let ls'		= ls & over rootModule (++ pathDiff)
					& over rootPath (++ ("/" ++ intercalate "/" pathDiff))
		known'	<- _loadAll ls' (get importName imprt) |> get currentlyKnown
		ls & set currentlyKnown known' & return
 | otherwise
	= _loadAll ls (get importName imprt)






searchingBehaviour
      = [ "The name of a language (within the document, with whitespace removed) should be the same as the filename (which should not contain whitespace)."
	, "This name is _still_ case sensitive."
	, "Paths are searched for relative from the root; by replacing the dots by file seperators."
	, "However, when imports are marked with 'local', then the module is resovled against the path in which the module was found, acting as a new root module."
	, "This offers a convenient way for package management, as modules within a package can not access other packages in the top level"
	] & unwords
