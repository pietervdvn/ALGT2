module Lib
    ( someFunc
    ) where

import Utils.All

import Assets
import AssetUtils

import LanguageDef.LanguageDef
import LanguageDef.ModuleLoader	
import LanguageDef.Function
import LanguageDef.Expression 
import LanguageDef.Utils.LocationInfo
import LanguageDef.Utils.Scope
import LanguageDef.Interpreter
import LanguageDef.Syntax


import LanguageDef.API
import LanguageDef.LangDefs



import Data.Map as M


import Utils.Version

someFunc :: IO ()
someFunc = putStrLn versionString
