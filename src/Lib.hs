module Lib
    ( someFunc
    ) where

import Utils.Utils

import LanguageDef.LanguageDef
import LanguageDef.ModuleLoader	
import Assets
import AssetUtils

import Utils.Version

someFunc :: IO ()
someFunc = putStrLn versionString
