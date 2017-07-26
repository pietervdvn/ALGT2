module Lib
    ( someFunc
    ) where

import Utils.Utils

import LanguageDef.LanguageDef
import LanguageDef.ModuleLoader	
import LanguageDef.MetaFunction
import LanguageDef.MetaExpression 
import LanguageDef.Scope

import Data.Map as M




import Assets
import AssetUtils

import Utils.Version

someFunc :: IO ()
someFunc = putStrLn versionString
