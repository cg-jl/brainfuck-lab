{-# LANGUAGE TupleSections #-}

module Lib where

-- module Lib (runFile) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Char (chr, ord)
import Data.Either
import Data.Functor
import Data.List
import Data.Maybe
import System.IO (getChar, hFlush, hPutStrLn, putChar, stderr, stdout)

-- check all '[' match.

data SyntaxError = MissingLB Int | MissingRB Int

instance Show SyntaxError where
  show (MissingLB pos) = "Missing opening '[' for ']' at " ++ show pos
  show (MissingRB pos) = "Missing closing ']' for '[' at " ++ show pos

data CheckerState = CheckerState {currentPos :: Int, posStack :: [Int]}

addCurrentPosToStack :: CheckerState -> CheckerState
addCurrentPosToStack (CheckerState pos stack) = CheckerState pos (pos : stack)

incCurrentPos :: CheckerState -> CheckerState
incCurrentPos (CheckerState pos stack) = CheckerState (pos + 1) stack

checkSyntax :: String -> Either SyntaxError String
checkSyntax xs = evalStateT (checkSyntax' xs) (CheckerState 0 []) $> xs
  where
    checkSyntax' :: String -> StateT CheckerState (Either SyntaxError) ()
    checkSyntax' xs = mapM_ (checkChar >=> const (modify' incCurrentPos)) xs >> checkForMissingRB

    popStack :: StateT CheckerState (Either SyntaxError) ()
    popStack = do
      stack <- gets posStack
      when (null stack) missingLeft
      modify $ \state -> state {posStack = tail stack}

    checkChar :: Char -> StateT CheckerState (Either SyntaxError) ()
    checkChar '[' = modify addCurrentPosToStack
    checkChar ']' = popStack
    checkChar _ = return ()

    checkForMissingRB = do
      xs <- gets posStack
      unless (null xs) (missingRight xs)

    missingRight = lift . Left . MissingRB . head
    missingLeft = gets currentPos >>= lift . Left . MissingLB

-- compiling it down.
data BFCommand
  = GoLeft
  | GoRight
  | Increment
  | Decrement
  | Print
  | Read
  | Loop [BFCommand]

instance Show BFCommand where
  show GoLeft = "<"
  show GoRight = ">"
  show Increment = "+"
  show Decrement = "-"
  show Print = "."
  show Read = ","
  show (Loop xs) = "[" ++ concatMap show xs ++ "]"

-- find the companion '[' on nested loops.
findLoop :: String -> (String, String)
findLoop = findLoop' 0
  where
    ---     balance
    findLoop' :: Int -> String -> (String, String)
    -- if balance is zero and I got ']', I've finished.
    findLoop' 0 (']' : rest) = ("", rest)
    -- if I got ']' and balance is nonzero, it was from other loop.
    findLoop' b (']' : rest) = let (p, q) = findLoop' (b - 1) rest in (']' : p, q)
    -- if I get '[' then I enter a new loop, therefore balance increments.
    findLoop' b ('[' : xs) = let (p, q) = findLoop' (b + 1) xs in ('[' : p, q)
    -- standard case wih anything, just skip.
    findLoop' b (x : xs) = let (p, q) = findLoop' b xs in (x : p, q)

process :: (a -> b) -> (a -> b -> a) -> State b x -> State a x
process get' set' sa = do
  state <- get
  let (o, q) = runState sa (get' state)
      state' = set' state q

  put state' >> return o

processL :: Lens comp sub -> State sub a -> State comp a
processL lens = process (view lens) (set lens)

sndL :: Lens (a, b) b
sndL = Lens snd $ \(a, _) b -> (a, b)

-- readS :: Reader r a -> State r a
-- readS = gets . runReader

advance :: State [a] (Maybe a)
advance = gets uncons >>= maybe (return Nothing) (\(a, xs) -> put xs $> Just a)

findCompanionLoop' :: State (Int, String) String
findCompanionLoop' = processL sndL advance >>= maybe (return []) loopChar
  where
    loopChar ']' = do
      balance <- gets fst
      if balance == 0
        then return []
        else modify (\(a, b) -> (a - 1, b)) >> (']' :) <$> findCompanionLoop'
    loopChar '[' = modify (\(a, b) -> (a + 1, b)) >> ('[' :) <$> findCompanionLoop'
    loopChar x = (x :) <$> findCompanionLoop'

findCompanionLoop :: State String String
findCompanionLoop = process (0,) (const snd) findCompanionLoop'

compile :: String -> Either SyntaxError [BFCommand]
-- essential to check the syntax before compiling things like loops and such.
compile xs = compile' <$> checkSyntax xs
  where
    runTillNoInput :: (a -> State [a] b) -> State [a] [b]
    runTillNoInput f = advance >>= maybe (return []) (flip (liftA2 (:)) (runTillNoInput f) . f)

    compile' :: String -> [BFCommand]
    compile' = catMaybes . evalState (runTillNoInput (runMaybeT . compileChar))

    compileChar :: Char -> MaybeT (State String) BFCommand
    compileChar '.' = return Print
    compileChar ',' = return Read
    compileChar '+' = return Increment
    compileChar '-' = return Decrement
    compileChar '>' = return GoRight
    compileChar '<' = return GoLeft
    compileChar '[' = Loop . compile' <$> lift findCompanionLoop
    compileChar _ = fail "we don't care about other characters"

-- compile' :: String -> [BFCommand]
-- compile' [] = []
-- compile' ('.' : xs) = Print : compile' xs
-- compile' (',' : xs) = Read : compile' xs
-- compile' ('+' : xs) = Increment : compile' xs
-- compile' ('-' : xs) = Decrement : compile' xs
-- compile' ('>' : xs) = GoRight : compile' xs
-- compile' ('<' : xs) = GoLeft : compile' xs
-- -- find the companion ']' and parse the internal loop, also compile the rest.
-- compile' ('[' : xs) = let (p, q) = findLoop xs in Loop (compile' p) : compile' q
-- compile' (x : xs) = compile' xs

data Tape = Tape {pivot :: Int, left :: [Int], right :: [Int]}

mkTape :: Tape
mkTape = Tape 0 z z where z = repeat 0

runMachine :: [BFCommand] -> StateT Tape IO ()
runMachine = mapM_ runOne

data Lens a b = Lens {view :: a -> b, set :: a -> b -> a}

-- like 'modify' but for Lens.
over :: Lens a b -> (b -> b) -> a -> a
over lens f a =
  let v = view lens a
   in set lens a (f v)

tapePivot :: Lens Tape Int
tapePivot = Lens pivot $ \tape newpv -> tape {pivot = newpv}

runOne :: BFCommand -> StateT Tape IO ()
runOne GoLeft = modify moveTapeLeft
runOne GoRight = modify moveTapeRight
runOne Print = gets pivot >>= lift . (putChar >=> const flush) . chr
  where
    flush = hFlush stdout
runOne Read = lift getChar >>= modify . flip (set tapePivot) . ord
runOne Increment = modify $ over tapePivot incWrapped
  where
    incWrapped :: Int -> Int
    incWrapped 255 = 0
    incWrapped a = a + 1
runOne Decrement = modify $ over tapePivot decWrapped
  where
    decWrapped :: Int -> Int
    decWrapped 0 = 255
    decWrapped a = a - 1
runOne l@(Loop xs) = do
  let doLoop = runMachine xs
      again = runOne l

  p <- gets pivot
  unless (p == 0) (doLoop >> again)

moveTapeLeft :: Tape -> Tape
moveTapeLeft (Tape p ls (r : rs)) = Tape r (p : ls) rs

moveTapeRight :: Tape -> Tape
moveTapeRight (Tape p (l : ls) rs) = Tape l ls (p : rs)

run :: [BFCommand] -> IO ()
run = flip evalStateT mkTape . runMachine

handleError :: Show a => a -> IO ()
handleError = hPutStrLn stderr . ("Error: " ++) . show

runFile :: String -> IO ()
runFile = readFile >=> either handleError run . compile
