module Interpreter (run) where

import Control.Monad.State
import Data.Char
import Data.Word
import Instructions
import System.IO (hFlush, putChar, stdout)
import Util

data Tape = Tape {pivot :: Word8, left :: [Word8], right :: [Word8]}

newTape :: Tape
newTape = Tape 0 z z where z = Prelude.repeat 0

runAll :: [BFCommand] -> StateT Tape IO ()
runAll = mapM_ runOne

pivotL :: Lens Tape Word8
pivotL = Lens pivot $ \t pivot -> t {pivot = pivot}

moveTapeLeft :: Tape -> Tape
moveTapeLeft (Tape p lefts (r : rights)) = Tape r (p : lefts) rights

moveTapeRight :: Tape -> Tape
moveTapeRight (Tape p (l : lefts) rights) = Tape l lefts (p : rights)

runOne :: BFCommand -> StateT Tape IO ()
runOne GoLeft = modify' moveTapeLeft
runOne GoRight = modify' moveTapeRight
runOne Print = do
  p <- gets pivot
  lift $ putChar (chr $ fromIntegral p) >> hFlush stdout
runOne Read = do
  c <- lift getChar
  modify $ update' pivotL (fromIntegral $ ord c)
runOne Increment = modify $ over pivotL (+ 1)
runOne Decrement = modify $ over pivotL (\a -> a - 1)
runOne i@(Loop inside) = do
  let runLoop = runAll inside
      thisAgain = runOne i

  p <- gets pivot
  unless (p == 0) $ runLoop >> thisAgain


run :: [BFCommand] -> IO ()
run xs = evalStateT (runAll xs) newTape
