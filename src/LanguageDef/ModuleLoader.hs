{-# LANGUAGE TemplateHaskell #-}
module LanguageDef.ModuleLoader where

{- Responsible for finding and loading the appropriate language files -}

import Prelude hiding (putStr, putStrLn, readFile, putStrLn, fail)

import Utils.All
import Utils.PureIO

import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Data.LanguageDef
import LanguageDef.LangDefs
import LanguageDef.LangDefsFix

import Data.Map as M

import Control.Monad hiding (fail)

import qualified Assets





data LoadingStatus = LS
	{ _rootModule	:: [Name]
	, _rootPath	:: FilePath
	, _currentlyKnown	:: Map [Name] (FilePath, Failable (LanguageDef' () ()))
	}
makeLenses ''LoadingStatus



{- | Load all modules, given the root path where to start looking

>>> import AssetUtils
>>> lds = runPure allAssets' (loadAll "" ["TestInput","Nested","L"]) |> fst
>>> lds & either (error "hi")  isSuccess
True
>>> Right (Success ldScope) = lds
>>> langL = get environment ldScope ! ["TestInput", "Nested", "L"]
>>> (resolve langL syntaxCall) ([], "a")
Success (["TestInput","Nested","L"],"a")
>>> (resolve langL syntaxCall) ([], "x")
Success (["TestInput","Nested","X"],"x")

>>> runPure allAssets' (loadAll "TestInput" ["LoopingSupertypes"]) & either error id & fst & toCoParsable
"| While constructing the global supertyping relationship while typing \nError: \n  \8226 Cycles are detected in the supertype relationship of the syntaxes:  LoopingSupertypes.z \8835 LoopingSupertypes.y \8835 LoopingSupertypes.x \8835 LoopingSupertypes.z\n    LoopingSupertypes.bool \8835 LoopingSupertypes.bool"


-}


loadAll	:: FilePath -> [Name] -> PureIO (Failable LDScope)
loadAll fp plzLoad
	= do	defs	<- _loadAll (LS [] fp M.empty) plzLoad |> get currentlyKnown
		return $ _fixAll plzLoad defs
		

_fixAll	:: [Name] -> Map [Name] (FilePath, Failable (LanguageDef' () ())) -> Failable LDScope
_fixAll plzLoad defs
	= do	defs'	<- defs |> sndEffect & allGoodMap
		let defsFixedImports	= _fixImports defs'
		scopes	<- asLangDefs defsFixedImports
		checkExists' plzLoad scopes ("Scope "++dots plzLoad ++ " not found")


-- A few files (notably the ALGT-package) is builtin. This is the content of those files
injectedFiles	:: Map [Name] String
injectedFiles
	= M.fromList
		[ (["ALGT", "Builtins"], Assets._ALGT_Builtins_language)
		]

		

-- Adds the filepath to the imports, within the language def data
_fixImports	:: Map [Name] (FilePath, LanguageDef' () fr) -> Map [Name] (LanguageDef' ResolvedImport fr)
_fixImports langDefs
	= let	resolver	= langDefs |> fst in
		langDefs |> snd |> fixImport resolver |> either error id



_loadAll	:: LoadingStatus -> [Name] -> PureIO LoadingStatus
_loadAll ls toLoad
 | toLoad `member` get currentlyKnown ls
	= do	putStrLn $ "Already loaded "++dots toLoad
		return ls
 | otherwise
	= do	let path	= ((get rootPath ls : toLoad) & intercalate "/") ++ ".language"
		let msgs	= [ "Target", intercalate "." toLoad
				  , "Root filepath", get rootPath ls
				  , "Relative namespace", intercalate "." (get rootModule ls)
				  , "Target path", path
				  ] & perTwo (\a b -> "  "++a ++ ": "++b) & intercalate "\n"
		let isInjected	= toLoad `member` injectedFiles
		let msg	= "\nLoading:\n"++msgs
		if isInjected then putStrLn ("Loading builtin module "++dots toLoad) else putStrLn msg
		
		contents	<- if isInjected then 
					return $ Just (injectedFiles ! toLoad)
					else safeReadFile path

		let loadedLD	= _handleFile (get rootModule ls) toLoad path contents
		let absName	= get rootModule ls ++ toLoad
		let ls'	= ls & over currentlyKnown (M.insert absName (path, loadedLD |> fst))

		loadedLD |> snd & handleFailure (\e -> putStrLn (toParsable e) >> return ls')   
			(foldM (_loadImport toLoad) ls')


_handleFile	:: [Name] -> [Name] -> FilePath -> Maybe String -> Failable (LanguageDef' () (), [Import ()])
_handleFile _ _ toLoad Nothing
	= fail $ "The file "++toLoad++" was not found"
_handleFile root fq path (Just contents)
	= do	ld		<- parseFullFile path contents
		let ld'		= ld & resolveLocalImports (root, init fq {- The second parameter is the relative offset; thus init -})
		let toLoad	= ld & get langImports
		return (ld', toLoad)

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
