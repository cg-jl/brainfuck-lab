module Compile where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Functor
import Instructions
import Util

data FinderState = FinderState {finderBalance :: Int, finderSource :: String}

balanceL :: Lens FinderState Int
balanceL = Lens finderBalance $ \finder b -> finder {finderBalance = b}

sourceL :: Lens FinderState String
sourceL = Lens finderSource $ \finder s -> finder {finderSource = s}

findBalancedBracketRaw :: State FinderState String
findBalancedBracketRaw = runTillNothingL sourceL (\a -> processNext a $> a)
  where
    processNext :: Char -> MaybeT (State FinderState) ()
    processNext ']' = do
      balance <- gets finderBalance
      if balance == 0
        then fail "nothing else to do" -- finished
        else lift $ modify $ over balanceL pred -- continue
    processNext '[' = lift $ modify $ over balanceL succ
    processNext _ = return ()

findBalancedBracket :: State String String
findBalancedBracket = processWith toFinderState findBalancedBracketRaw
  where
    toFinderState = Lens (FinderState 0) (const finderSource)

compileChar :: Char -> MaybeT (State String) BFCommand
compileChar '.' = return Print
compileChar ',' = return Read
compileChar '+' = return Increment
compileChar '-' = return Decrement
compileChar '>' = return GoRight
compileChar '<' = return GoLeft
compileChar '[' = Loop . compile <$> lift findBalancedBracket
compileChar _ = fail "we dont' care about other characters"

compile :: String -> [BFCommand]
compile = evalState (runTillNothing compileChar)


