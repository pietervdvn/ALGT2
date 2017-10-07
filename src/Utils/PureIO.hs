 {-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
module Utils.PureIO (PureIO', PureIO
	, isApplicative, toApplicative, calculateEffects
	, runIO, runIO', runPure', runPure, runPureStatus
	, readFile, getLine, getFile, putStr, putStrLn, fail
	, readFile', getLine', getFile', putStr', putStrLn'
	, doesFileExist', doesFileExist, safeReadFile
	) where

import Utils.All

import Prelude hiding (writeFile, putStrLn, readFile, readLine, getLine, putStr)
import qualified Prelude as IO
import qualified System.Directory as IO
import Data.Map as M

import Data.Bifunctor (first)

{-
PureIO' is an explicit representation of the 'real' IO monad in an applicative way.
This allows to _lie_ a little to the implementation...
This is the applicative branch, which allows some automatic reasoning
-}

data PureIO' b
	= Print String b-- Print to stdOut; without newline; the extra parameter is returned (forces typechecking)
	| Read (String -> b)	-- reads a line from the terminal
	| Value b
	| forall a . Apply (PureIO' a) (PureIO' (a -> b))	-- while this can be written in terms of bind, an explicit apply allows for better automatic analysis
	| ReadFile FilePath (String -> b)	-- Read a file from the FS
	| FileExists FilePath (Bool -> b)
	| WriteFile FilePath String b
	| Fail String

{-
Allows monadic operations on PureIO'
-}
data PureIO b
	= ApplicIO (PureIO' b)
	| forall a . Bind (PureIO a) (a -> PureIO b)





-- Actually execute the PureIO'
runIO'	:: PureIO' b -> IO b
runIO' (Print str b)
	= do	IO.putStr str
		return b
runIO' (Read f)
	= IO.getLine |> f
runIO' (Value b)
	= return b
runIO' (Apply pioA pioA2b)
	= do	a	<- runIO' pioA
		a2b	<- runIO' pioA2b
		return $ a2b a
runIO' (FileExists pth f)
	= IO.doesFileExist pth |> f
runIO' (ReadFile pth f)
	= IO.readFile pth |> f
runIO' (WriteFile pth contents b)
	= do	IO.writeFile pth contents
		return b
runIO' (Fail str)
	= error str


runIO	:: PureIO b -> IO b
runIO (ApplicIO io)
	= runIO' io
runIO (Bind ioA a2ioB)
	= do	a	<- runIO ioA
		runIO (a2ioB a)



data IOState	= IOState 
	{ _inputResting	:: [String]
	, _output	:: [String]
	, _fileSystem 	:: Map Name String
	, _filesRead	:: [FilePath]
	, _filesWritten	:: [FilePath]
	}
makeLenses ''IOState


runPure'	:: ([String], Map Name String) -> PureIO' a -> Either String (a, [String])
runPure' input io
		= _runPure' (_asIOState input) io ||>> get output & first fst

_asIOState	:: ([String], Map Name String) -> IOState
_asIOState (inp, fs)
	= IOState inp [] fs [] []



_runPure'	:: IOState -> PureIO' a -> Either (String, IOState) (a, IOState)
_runPure' input (Print str b)
	= (b, input & over output (++[str])) & return
_runPure' input (Read f)	-- Read line
	= case get inputResting input of
		(inp:_)	-> return (f inp, input & over inputResting tail)
		[]	-> Left ("Insufficient input", input)
_runPure' input (Value b)
	= (b, input) & return
_runPure' input (Apply pioA pioA2B)
	= do	(a, input0)	<- _runPure' input pioA
		(f, input1)	<- _runPure' input0 pioA2B
		return (f a, input1)
_runPure' input (FileExists pth f)
	= do	let exists	= pth `member` (get fileSystem input)
		return (f exists, input & over filesRead (pth:))
_runPure' input (ReadFile pth f)
	= do	contents	<- checkExists pth (get fileSystem input) ("No file \""++pth++"\" found in the input")
					& first (\msg -> (msg, input))
		return (f contents, input & over filesRead (pth:))
_runPure' input (WriteFile pth contents b)
	= do	let fs'	= M.insert pth contents (input & get fileSystem)
		return (b, input & set fileSystem fs' & over filesWritten (pth:))
_runPure' input (Fail str)
	= Left (str, input)




runPure		:: ([String], Map Name String) -> PureIO a -> Either String (a, [String])
runPure input io
		= _runPure (_asIOState input) io & first fst ||>> get output 


-- Runs the given IO pure, but prints output + status reports afterwards
runPureStatus	:: ([String], Map Name String) -> PureIO a -> IO a
runPureStatus input io
	= do	let (state, a)	= _runPure (_asIOState input) io
						& either	(\(msg, state) -> (state, Left msg)) 
								(\(  a, state) -> (state, Right a))
		IO.putStrLn (_report state)
		state & get output |> indentWith "|   " & intercalate "\n" & IO.putStrLn
		a & either error return 


_report	:: IOState -> String
_report state
	= [_filesReport "accessed" $ get filesRead state
	  , _filesReport "modified" $ get filesWritten state] & intercalate "\n"


_filesReport	:: String -> [FilePath] -> String
_filesReport option []
	= "No files "++option
_filesReport option files
	= let	trail	= if length files > 10 then "..." else "" in
		[ show (length files) ++" " ++ option++":"
		, files & reverse & take 10 & (++[trail]) |> indent & intercalate "\n"
		] & intercalate "\n"


_runPure	:: IOState -> PureIO a -> Either (String, IOState) (a, IOState)
_runPure input (ApplicIO io)
		= _runPure' input io
_runPure input (Bind actA a2actB)
		= do	(a, input0)	<- _runPure input actA
			(b, input1)	<- _runPure input0 (a2actB a)
			return (b, input1)


instance Functor PureIO' where
	fmap f action
		= Apply action (Value f)
instance Applicative PureIO' where
	pure	= Value
	(<*>)	= flip Apply



instance Functor PureIO where
	fmap f (ApplicIO pio)
		= ApplicIO (pio |> f)
	fmap f (Bind actA a2actB)
		= Bind actA (\a -> a2actB a |> f)


instance Applicative PureIO where
	pure	= ApplicIO . pure
	(<*>) (ApplicIO a2b) (ApplicIO a)
		= ApplicIO (a2b <*> a)
	(<*>) a2b a
		= Bind a2b (\f -> Bind a (pure . f))


instance Monad PureIO where
	return a
		= ApplicIO $ Value a
	(>>=)	= Bind
	fail	= ApplicIO . Fail

-- | Checks that a PureIO only uses applicative properties, so that automatic reasoning can be used
-- >>> isApplicative $ putStr "Hi"
-- True
-- >>> isApplicative $ reverse <$> getLine
-- True
-- >>> isApplicative $ getLine >>= readFile
-- False
isApplicative	:: PureIO a -> Bool
isApplicative (Bind _ _)
		= False
isApplicative _	= True


toApplicative	:: PureIO a -> Maybe (PureIO' a)
toApplicative (ApplicIO io)
		= Just io
toApplicative _
		= Nothing


-- | Calculate what files are read and written
-- Not possible if the PureIO' is not applicative
calculateEffects	:: PureIO' a -> ([FilePath], [FilePath])
calculateEffects (Apply actA actA2B)
	= _mergeEffects (calculateEffects actA) (calculateEffects actA2B)
calculateEffects (ReadFile fp _)
	= ([fp], [])
calculateEffects (FileExists fp _)
	= ([fp], [])
calculateEffects (WriteFile fp _ _)
	= ([], [fp])
calculateEffects _
	= ([], [])


_mergeEffects (read, written) (read', written')
	= (read ++ read', written ++ written')

getLine'	:: PureIO' String
getLine'	= Read id


readFile'	:: FilePath -> PureIO' String
readFile' str	= ReadFile str id

getFile'	:: FilePath -> PureIO' String
getFile'	= readFile'

doesFileExist'	:: FilePath -> PureIO' Bool
doesFileExist' str
		= FileExists str id


safeReadFile	:: FilePath -> PureIO (Maybe String)
safeReadFile fp
	= do	doesEx	<- doesFileExist fp
		if doesEx then
			readFile fp |> Just
		else	return Nothing


putStr'		:: String -> PureIO' ()
putStr' str	= Print str ()

putStrLn'	:: String -> PureIO' ()
putStrLn' str	= putStr' (str ++ "\n")


getLine		= ApplicIO getLine'
getFile		= _monad getFile'
putStr		= _monad putStr'
putStrLn	= _monad putStrLn'
readFile	= _monad readFile'
doesFileExist	= _monad doesFileExist'


_monad		:: (b -> PureIO' a) -> b -> PureIO a
_monad applicIO
		= ApplicIO . applicIO

ret		:: a -> PureIO' a
ret		= Value




