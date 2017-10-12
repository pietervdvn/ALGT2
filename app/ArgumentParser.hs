 {-# LANGUAGE TemplateHaskell #-}
module ArgumentParser where

{-
This module defines parsing of the arguments and reading/writing of the config file
-}

import qualified Assets

import Utils.All
import Utils.PureIO

import Data.Monoid ((<>))
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Data.Maybe
import qualified Data.Map as M

import Options.Applicative
import Control.Monad

import qualified Data.ByteString as B




descriptionText	= "ALGT (Automated Language Generation Tool)- automatically parse, interpret and proof properties of aribtrary languages.\n\n"++
			"This tool parses a '.language'-file, where the syntax of your language is defined. With this, your target file is parsed."++
			"In the language file, rewrite rules (or functions) can be defined - or better, rules defining properties can be defined."++
			"These can be applied to your target language to interpret, typecheck or proof some other property."

showVersion (v, vId)	= (v |> show & intercalate ".") ++", "++show vId
headerText v
		= "Automated Language Generation Tool (version "++ showVersion v ++" )"


class ActionSpecified a where
	actionSpecified	:: a -> Bool

-- Arguments which stay locally, such as printing the version number
data MainArgs	= MainArgs 
			{ showVersionNr	:: () -> ()
			, args		:: Args
			}


instance ActionSpecified MainArgs where
	actionSpecified args
		= False

data Args	= Args 
	{ _replOpts	:: Maybe ReplOpts
	, _dumpTemplate	:: Bool
	}


data ReplOpts	= ReplOpts
	{ _replPath	:: FilePath
	, _replModule	:: String
	}

makeLenses ''Args
makeLenses ''ReplOpts

parseArgs	:: ([Int], String) -> [String] -> IO Args
parseArgs version strs	
	= do	let result	= execParserPure defaultPrefs (parserInfo version) strs
		MainArgs doShowVersion args
				<- handleParseResult result
		return $ doShowVersion ()
		return args


parserInfo v	= parserInfo' (mainArgs $ showVersion v) v

parserInfo' parser v
	= info (helper <*> parser)
			(fullDesc <> progDesc descriptionText <> header (headerText v))



mainArgs versionMsg
	= MainArgs <$> infoOption versionMsg
			(long "version"
			<> short 'v'
			<> help "Show the version number and text")
		   <*> argsP



argsP	= Args <$> optional replOptsP
		<*> switch
			(long "template"
			<> help "Save a template to a local file")	


replOptsP	
	= ReplOpts <$> 	argument str
			(metavar "Filepath"
			<> help "The filepath to the root of the module"
			<> action "file")
		<*> argument str
			(metavar "module"
			<> help "The module to load")
