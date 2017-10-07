module Utils.GetLine where

{- A get line where backspace and arrows work -}


import System.IO
import System.Console.ANSI


prompt	:: String -> IO String
prompt msg
	= prompt' msg ""
		

prompt'	:: String -> String -> IO String
prompt' msg curText
	= do	clearLine
		putStr "\r"
		putStr msg
		putStr curText
		hFlush stdout
		c	<- getChar
		case c of
			'\DEL'	-> prompt' msg (init curText)
			'\f'	-> clearScreen >> setCursorPosition 0 0 >> prompt' msg curText
			'\EOT'	-> putStrLn "" >> return (curText ++ [c])
			'\n'	-> return curText
			'\ESC'	-> do	getChar
					getChar
					prompt' msg curText
			_	-> prompt' msg (curText ++ [c])
			
