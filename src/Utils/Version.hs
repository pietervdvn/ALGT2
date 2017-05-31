module Utils.Version where

{- 
Contains a version number and version string
 -}

import Paths_ALGT2 (version)
import Utils.Utils

import Data.Version


versionMessage	= "Hello World, again"
versionString	= showVersion version ++ " (" ++ versionMessage ++ ")"
