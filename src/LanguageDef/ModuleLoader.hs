{-# LANGUAGE TemplateHaskell #-}
module LanguageDef.ModuleLoader (loadAll, loadAll', Cache) where

{- Responsible for finding and loading the appropriate language files -}

import Prelude hiding (putStr, putStrLn, readFile, putStrLn, fail)
import qualified Assets

import Utils.All
import Utils.PureIO

import LanguageDef.Utils.ExceptionInfo
import LanguageDef.Data.LanguageDef
import LanguageDef.LangDefs
import LanguageDef.LangDefsFix

import Data.Map as M
import Data.Time.Clock (UTCTime)
import Data.Maybe

import Control.Monad hiding (fail)
import Control.Monad.State (StateT, runStateT)
import qualified Control.Monad.State as State

import Lens.Micro hiding ((&))


type Cache	= Map FilePath (UTCTime, LanguageDef' () ())
type Dirty	= Bool

data LoadingStatus = LS
	{ _rootPath		:: FilePath
	, _currentlyKnown	:: Map [Name] (FilePath, Failable (LanguageDef' () ()))
	-- As type resolution and type checking is done when all modules are loaded, we don't have to keep a dependency graph
	, _lastModTimes		:: Map FilePath UTCTime
	, _cache		:: Cache
	}
makeLenses ''LoadingStatus


type LoadingIO a	= StateT LoadingStatus PureIO a


{- | Load all modules, given the root path where to start looking

>>> import AssetUtils
>>> lds = runPure allAssets' (loadAll "" ["TestInput","Nested","L"]) |> fst
>>> lds & either error id & fst & handleFailure (error . toParsable) (const True)
True
>>> Right (Success ldScope, _) = lds
>>> langL = get environment ldScope ! ["TestInput", "Nested", "L"]
>>> (resolve langL syntaxCall) ([], "a")
Success (["TestInput","Nested","L"],"a")
>>> (resolve langL syntaxCall) ([], "x")
Success (["TestInput","Nested","X"],"x")

>>> runPure allAssets' (loadAll "TestInput" ["LoopingSupertypes"]) & either error fst & fst & toCoParsable
"| While constructing the global supertyping relationship while typing \nError: \n  \8226 Cycles are detected in the supertype relationship of the syntaxes:  LoopingSupertypes.z \8835 LoopingSupertypes.y \8835 LoopingSupertypes.x \8835 LoopingSupertypes.z\n    LoopingSupertypes.bool \8835 LoopingSupertypes.bool"


-}
loadAll	:: FilePath -> [Name] -> PureIO (Failable LDScope, Cache)
loadAll	= loadAll' M.empty

loadAll'	:: Cache -> FilePath -> [Name] -> PureIO (Failable LDScope, Cache)
loadAll' cach fp plzLoad
	= do	let startState	=  LS fp M.empty M.empty cach
		loadingSt	<- runStateT (_loadAll plzLoad) startState |> snd
		let lds		= _fixAll plzLoad loadingSt
		let cache'	= _extractNewCache loadingSt
		return (lds, cache')


_fixAll	:: [Name] -> LoadingStatus -> Failable LDScope
_fixAll plzLoad ls
	= do	defs			<- ls & get currentlyKnown |> sndEffect & allGoodMap
		defsFixedImports	<- _fixImports defs
		scopes			<- asLangDefs defsFixedImports
		checkExists' plzLoad scopes ("Scope "++dots plzLoad ++ " not found")

		

-- Adds the filepath to the imports, within the language def data
_fixImports	:: Map [Name] (FilePath, LanguageDef' () fr) -> Failable (Map [Name] (LanguageDef' ResolvedImport fr))
_fixImports langDefs
	= let	resolver	= langDefs |> fst in
		langDefs |> snd |> fixImport resolver & allGoodMap


_extractNewCache	:: LoadingStatus -> Cache
_extractNewCache ls
	= let	cach'	= ls & get currentlyKnown & M.elems 
					|> sndEffect
					& successess
					& M.fromList	:: Map FilePath (LanguageDef' () ())

		cache'
			= M.intersectionWith (,) (get lastModTimes ls) cach'
		in
		cache'
		


-- A few files (notably the ALGT-package) is builtin. This is the content of those files
-- >>> injectedFiles
-- M.fromList ...
injectedFiles	:: Map [Name] (LanguageDef' () ())
injectedFiles
      = [ (["ALGT", "Builtins"], Assets._ALGT_Builtins_language)
	]	|> (\(key, contents) -> (key, contents & parseFullFile (dots key)) )
		|> sndEffect & allGood & crash'
		& M.fromList |> resolveLocalImports []





_loadAll	:: [Name] -> LoadingIO ()
_loadAll toLoad
	= do	newToLoads	<- loadFile' toLoad
		newToLoads |+> _loadAll
		pass



{-
Same as loadFile, except that the file is not loaded if already present in the 'currentlyKnown'.

If the file is already loaded, an empty list is returned

-}
loadFile'	:: [Name] -> LoadingIO [[Name]]
loadFile' toLoad
	= do	alreadyLoaded	<- get' currentlyKnown |> (toLoad `M.member`)
		if alreadyLoaded then
			return []
		else
			loadFile toLoad

{-

Loads the file, either from an injected file, cache or HDD. Saves it in the LoadingStatus, including the modification times.

Returns the direct dependencies of the language dep (module-names that are imported) if loading was successfull

-}
loadFile	:: [Name] -> LoadingIO [[Name]]
loadFile toLoad
	= do	let mInjected	= loadBuiltin toLoad
		mCached		<- loadFromCache toLoad
		(lastModTime, ld)	
				<- firstJust mInjected mCached & maybe 
					(loadFromDisk toLoad) {- :: LoadingInfo (Failable (LanguageDef' () ())) -} 
					(\(time, ld) -> return (time, return ld)) {- (UTCTime, LD) -> LoadingInfo (Failable LD) -}
		fp		<- filepathFor toLoad
		insert' currentlyKnown toLoad (fp, ld)
		insert' lastModTimes fp lastModTime
		handleFailure
			(const $ return [])
			(\ld -> ld & get langImports |> fqForImport toLoad & return) ld



{-
Loads a languagedef directly from the disk. Includes the parsing.
Totally ignores caching
-}
loadFromDisk	:: [Name] -> LoadingIO (UTCTime, Failable (LanguageDef' () ()))
loadFromDisk toLoad
	= do	fp		<- filepathFor toLoad
		contents	<- State.lift $ readFile fp
		lastModTime	<- State.lift $ getModificationTime fp |> fromJust
		(lastModTime, parseFullFile fp contents |> resolveLocalImports (init toLoad)) & return


{-
If
	- The file is in the cache
	- and is not modified on HDD (implies that the file exists)
Then it returns the languagedef from the cache as (Just ld)
-}
loadFromCache	:: [Name] -> LoadingIO (Maybe (UTCTime, LanguageDef' () ()))
loadFromCache toLoad
	= do	cach	<- get' cache
		fp	<- filepathFor toLoad
		lastTimeMod	<- State.lift $ getModificationTime fp
		return $ do	lastTimeMod'		<- lastTimeMod
				ld@(cacheModTime, _)	<- M.lookup fp cach
				if cacheModTime == lastTimeMod' then
					return ld
				else Nothing


loadBuiltin	:: [Name] -> Maybe (UTCTime, LanguageDef' () ())
loadBuiltin toLoad
	= do	ld	<- M.lookup toLoad injectedFiles
		return (Assets.timeCreated, ld)


filepathFor	:: [Name] -> LoadingIO FilePath
filepathFor toLoad
	= do	rootPth		<- get' rootPath
		return (rootPth ++ "/" ++ intercalate "/" toLoad ++ ".language")



get'	:: Getting a LoadingStatus a -> LoadingIO a
get' lens
	= State.gets (get lens)


set'	:: ASetter LoadingStatus LoadingStatus a a -> a -> LoadingIO ()
set' lens a
	= State.modify (set lens a)

over'	:: ASetter LoadingStatus LoadingStatus a b -> (a -> b) -> LoadingIO ()
over' lens f
	= State.modify (over lens f)

insert'	:: (Ord k) => ASetter LoadingStatus LoadingStatus (Map k v) (Map k v) -> k -> v -> LoadingIO ()
insert' lens k v
	= State.modify (over lens (M.insert k v))

