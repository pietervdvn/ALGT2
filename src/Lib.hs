module Lib
    ( someFunc
    ) where

import Utils.Utils

import LanguageDef.LanguageDef

import Utils.Version

import LanguageDef.Syntax
import LanguageDef.ParseTree

someFunc :: IO ()
someFunc = putStrLn $ versionString
