module Lib (runFile) where

import Control.Monad
import Data.Char (chr, ord)
import Data.Either
import System.IO (getChar, hFlush, hPutStrLn, putChar, stderr, stdout)

-- check all '[' match.

data SyntaxError = MissingLB Int | MissingRB Int

instance Show SyntaxError where
  show (MissingLB pos) = "Missing opening '[' for ']' at " ++ show pos
  show (MissingRB pos) = "Missing closing ']' for '[' at " ++ show pos

checkSyntax :: String -> Either SyntaxError String
checkSyntax = checkSyntax' (0, [])
  where
    checkSyntax' :: (Int, [Int]) -> String -> Either SyntaxError String
    -- no more to check, no more loops. have fun!
    checkSyntax' (_, []) [] = return []
    -- no more to check, but one unclosed loop. no fun for you! :(
    checkSyntax' (_, pos : _) [] = Left $ MissingRB pos
    checkSyntax' (pos, stack) (x : xs) =
      (x :) <$> case x of
        -- push loop
        '[' -> checkSyntax' (pos + 1, pos : stack) xs
        ']' -> case stack of
          -- stack empty -> bad closing.
          [] -> Left $ MissingLB pos
          -- pop from the stack.
          _ : ls -> checkSyntax' (pos + 1, ls) xs
        _ -> checkSyntax' (pos + 1, stack) xs

-- compiling it down.
data BFCommand
  = PointLeft
  | PointRight
  | -- use some easy optimization potential
    Increment Int
  | Decrement Int
  | -- this one's cool as well.
    Reset
  | Print
  | Loop [BFCommand]
  | Read
  deriving (Show)

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
compile xs = optimize . compile' <$> checkSyntax xs
  where
    compile' :: String -> [BFCommand]
    compile' [] = []
    compile' ('.' : xs) = Print : compile' xs
    compile' (',' : xs) = Read : compile' xs
    -- take as many '+' as possible and wrap them into a single command.
    compile' ('+' : xs) = let (p, q) = span (== '+') xs in Increment (length p + 1) : compile' q
    -- same with '-'.
    compile' ('-' : xs) = let (p, q) = span (== '-') xs in Decrement (length p + 1) : compile' q
    compile' ('>' : xs) = PointRight : compile' xs
    compile' ('<' : xs) = PointLeft : compile' xs
    -- find the companion ']' and parse the internal loop, also compile the rest.
    compile' ('[' : xs) = let (p, q) = findLoop xs in Loop (compile' p) : compile' q
    compile' (x : xs) = compile' xs

-- Optimizing!

optimize :: [BFCommand] -> [BFCommand]
optimize [] = []
optimize (Loop [Decrement 1] : cs) = Reset : optimize cs
optimize (x : xs) = x : optimize xs

-- Managing data.

-- left anchor-> | <-right anchor | currently pointed to.
data Buffer = Buffer [Int] [Int] Int

-- move pointer left -> move everything right.
moveLeft (Buffer (l : lefts) rights pivot) = Buffer lefts (pivot : rights) l

-- move pointer right -> move eveything left.
moveRight (Buffer lefts (r : rights) pivot) = Buffer (pivot : lefts) rights r

newBuffer = Buffer zeroes zeroes 0
  where
    zeroes = repeat 0

-- helpers for operations.

sumOverflow x y = (x + y) `mod` 256

subOverflow x y
  | x >= y = x - y
  | otherwise = abs $ (x - y) `mod` 256

-- Now the fun stuff: interpreting.

run :: Buffer -> [BFCommand] -> IO Buffer
run buf [] = return buf
run buf (PointLeft : cs) = run (moveLeft buf) cs
run buf (PointRight : cs) = run (moveRight buf) cs
run buf@(Buffer _ _ p) (Print : cs) = putChar (chr p) >> hFlush stdout >> run buf cs
run (Buffer l r _) (Read : cs) = getChar >>= (`run` cs) . Buffer l r . ord
run buf@(Buffer l r p) (Increment x : cs) = (`run` cs) . Buffer l r . sumOverflow p $ x
run buf@(Buffer l r p) (Decrement x : cs) = (`run` cs) . Buffer l r . subOverflow p $ x
run buf@(Buffer l r _) (Reset : cs) = run (Buffer l r 0) cs
run buf@(Buffer _ _ p1) loop@(Loop ys : xs) =
  -- if it's zero, skip it.
  if p1 == 0
    then run buf xs
    else do
      -- run the loop once.
      newbuf@(Buffer _ _ p2) <- run buf ys
      -- if the value it ended up in is not zero, run it again.
      -- else you can exit the loop.
      let runner = run newbuf in if p2 == 0 then runner xs else runner loop

runBF :: [BFCommand] -> IO ()
runBF = void . run newBuffer

handleError :: Show a => a -> IO () 
handleError = hPutStrLn stderr . ("Error: " ++) . show

runFile :: String -> IO ()
runFile = readFile >=> either handleError runBF . compile
