{-# LANGUAGE TemplateHaskell #-}
module LanguageDef.Tools.Scope where


import Utils.All
import Data.Map (Map)
import Data.Map as M


{- 
Represents a module scope, with elements of this module that are imported, exported and all relevant stuff
Arguments: 
name: The fully qualified names of modules, globally distinguishable
nameInt: the internal name for modules, as seen from inside (this allows renames, such as `import Data.Map as M`
a: the thing that is exported/imported by the module, the module contents. This will often be some data structure
importFlags: extra information about the imports, such as qualified imports and such
 -}
data Scope name nameInt a importFlags exportFlags = Scope
	{ _scopeName		:: name		-- The fully qualified scope name, as how it is called by the outside world
	, _imported		:: Map name importFlags	-- The fully qualified names of other scopes that are imported, the used importflags and remappings
	, _payload		:: a		-- The stuff that this module defines to the outside world, the actual payload
	, _reExports		:: Map name exportFlags	-- The stuff that is imported and reexported from other modules
	} deriving (Show, Eq, Ord)
makeLenses ''Scope


-- Gets a dictionary with modules available from this module (because they are imported)
importsFromEnv	:: (Ord name) => Map name a -> Scope name nameInt a flags eFlags -> Map name a
importsFromEnv env scope
	= M.intersectionWith const env (get imported scope)

