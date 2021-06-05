module Lib (runFile) where

import Control.Monad
import Control.Monad.State
import Data.Char (chr, ord)
import Data.Either
import Data.Functor
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
checkSyntax xs = runStateT (checkSyntax' xs) (CheckerState 0 []) $> xs
  where
    popStack :: StateT CheckerState (Either SyntaxError) ()
    popStack = do
      stack <- gets posStack
      when (null stack) $ gets currentPos >>= lift . Left . MissingLB
      modify $ \state -> state {posStack = tail stack}

    checkChar :: Char -> StateT CheckerState (Either SyntaxError) ()
    checkChar '[' = modify addCurrentPosToStack
    checkChar ']' = popStack
    checkChar _ = return ()

    checkSyntax' :: String -> StateT CheckerState (Either SyntaxError) ()
    checkSyntax' xs = mapM_ checkChar xs >> checkForMissingRB
      where
        checkForMissingRB = do
          xs <- gets posStack
          unless (null xs) $ lift $ Left $ MissingRB (head xs)

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

compile :: String -> Either SyntaxError [BFCommand]
-- essential to check the syntax before compiling things like loops and such.
compile xs = compile' <$> checkSyntax xs
  where
    compile' :: String -> [BFCommand]
    compile' [] = []
    compile' ('.' : xs) = Print : compile' xs
    compile' (',' : xs) = Read : compile' xs
    compile' ('+' : xs) = Increment : compile' xs
    compile' ('-' : xs) = Decrement : compile' xs
    compile' ('>' : xs) = GoRight : compile' xs
    compile' ('<' : xs) = GoLeft : compile' xs
    -- find the companion ']' and parse the internal loop, also compile the rest.
    compile' ('[' : xs) = let (p, q) = findLoop xs in Loop (compile' p) : compile' q
    compile' (x : xs) = compile' xs

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
runOne Print = gets pivot >>= lift . (putChar >=> const (hFlush stdout)) . chr
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
runOne l@(Loop xs) = gets pivot >>= branch
  where
    branch :: Int -> StateT Tape IO ()
    -- if pivot is zero, don't run the loop.
    branch 0 = return ()
    -- otherwise run the loop, and run this logic again.
    branch _ = runMachine xs >> runOne l

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
