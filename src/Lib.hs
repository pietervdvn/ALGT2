module Lib
    ( someFunc
    ) where

import Utils.Utils

import Assets
import AssetUtils

import LanguageDef.LanguageDef
import LanguageDef.ModuleLoader	
import LanguageDef.MetaFunction
import LanguageDef.MetaExpression 
import LanguageDef.LocationInfo
import LanguageDef.Scope
import LanguageDef.FunctionInterpreter
import LanguageDef.Syntax


import LanguageDef.API
import LanguageDef.LangDefs



import Data.Map as M


import Utils.Version

someFunc :: IO ()
someFunc = putStrLn versionString
