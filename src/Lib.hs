module Lib
    ( someFunc
    ) where

import Utils.Utils

import LanguageDef.LanguageDef

import Utils.Version

someFunc :: IO ()
someFunc = putStrLn $ versionString
