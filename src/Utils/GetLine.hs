{-# LANGUAGE TemplateHaskell #-}
module Utils.GetLine (prompt, prompt') where

{- A get line where backspace and arrows work -}

import Utils.All

import System.IO
import System.Console.ANSI

import qualified Control.Monad.State as St
import Control.Monad.State (StateT, liftIO, runStateT)
import Control.Monad
import Control.Arrow (first)

import Lens.Micro (ASetter, Getting, over)
import qualified Lens.Micro

import Data.Char
import Data.Maybe

data GetLineState	= GLS
	{ _curText	:: String
	, _msg		:: String
	, _showMsg	:: String -> IO ()
	, _relPos	:: Int	-- Position from the start from the string
	}
makeLenses ''GetLineState

type Action	= StateT GetLineState IO



prompt	:: String -> IO String
prompt msg
	= prompt' (msg, putStrLn)

prompt'	:: (String, String -> IO ()) -> IO String
prompt' (msg, showMsg)
	= do	let st	= GLS "" msg showMsg 0
		st'	<- runStateT _prompt st |> snd
		return $ get curText st'
		

_prompt	:: Action ()
_prompt
	= do	message		<- get' msg
		showMsgIO	<- get' showMsg
		currentText	<- get' curText
		liftIO $ setupPrompt (showMsgIO message) currentText
		setCursorPosition'
		c		<- liftIO getChar
		let action	= actions & filter (elem c . fst)
					& listToMaybe
					& maybe (defaultAction c) snd
		doContinue	<- action
		when doContinue _prompt


setupPrompt	:: IO () -> String -> IO ()
setupPrompt showMsg curText
	= do	clearLine
		putStr "\r"
		showMsg
		putStr curText
		hFlush stdout


actions	:: [(String, Action Bool)]
actions
      = [ ("\n", enterAction)
	, ("\EOT", eotAction)
	, ("\DEL", delAction)
	, ("\f", clearScrAct)
	, ("\ESC", escChar)
	, ("\NAK", clearLineAct)
	]




defaultAction	:: Char -> Action Bool
defaultAction c
	= do	(pre, post)	<- getBroken
		let str'	= pre ++ [c] ++ post
		put' curText str'
		over' relPos (+1)
		return True

enterAction	:: Action Bool
enterAction	= return False


delAction	:: Action Bool
delAction
	= do	(pre, post)	<- getBroken
		if null pre then
			put' curText post
		else do
			put' curText (init pre ++ post)
			over' relPos (+ (-1))
		return True


delAction'	:: Action Bool
delAction'
	= do	(pre, post)	<- getBroken
		if null post then
			put' curText pre
		else	put' curText (pre ++ tail post)
		return True


clearScrAct	:: Action Bool
clearScrAct
	= do	liftIO clearScreen
		liftIO $ setCursorPosition 0 0
		return True


eotAction	:: Action Bool
eotAction
	= do	put' curText "\EOT"
		liftIO $ putStrLn ""
		return False


clearLineAct	:: Action Bool
clearLineAct
	= do	put' curText ""
		put' relPos 0		
		return True



escChar		:: Action Bool
escChar
	= do	c	<- liftIO getChar
		selectEsc [c] c escSequences
		return True

selectEsc	:: String -> Char -> [(String, Action ())] -> Action ()
selectEsc log _ []
	= liftIO $ putStrLn $ "Prompt: Unknown escape sequence/keypress "++show log
selectEsc log c possActions
	= do	let possActions'	= possActions & filter ((==) c . head . fst)
						|> first tail
		let foundActions	= possActions' & filter (null . fst)
		if null foundActions then do
			c'	<- liftIO getChar
			selectEsc (log ++ [c']) c' possActions'
		else
			head foundActions & snd

escSequences	:: [(String, Action ())]
escSequences
      = [ ("[D", escLeftAction)
	, ("[C", escRightAction)
	, ("[3~", delAction' >> pass)
	]


escLeftAction	:: Action ()
escLeftAction
	= over' relPos (max 0 . (+ (-1)))

escRightAction	:: Action ()
escRightAction
	= do	lngth	<- get' curText |> length
		over' relPos (min lngth . (+ 1))



over' :: St.MonadState s m => ASetter s s a b -> (a -> b) -> m ()
over' lens f
	= St.modify (over lens f)


get'	:: St.MonadState s m => Getting a s a -> m a
get' lens
	= St.get |> get lens


put' :: St.MonadState s m => ASetter s s b b1 -> b1 -> m ()
put' lens v
	= St.modify (over lens (const v))
	
setCursorPosition' :: Action ()
setCursorPosition'
	= do	txt	<- get' curText
		m	<- get' msg
		let offset	= m & filter isPrint & length
		colPos	<- get' relPos |> (+ offset)		
		liftIO $ setCursorColumn colPos

getBroken
	= do	pos		<- get' relPos
		get' curText |> splitAt pos
		
