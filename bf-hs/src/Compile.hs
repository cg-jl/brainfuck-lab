module Compile (compile) where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Functor
import Instructions
import Util

findBalancedBracket :: State String String
findBalancedBracket = concat <$> runTillNothing nextChar
  where
    nextChar :: Char -> MaybeT (State String) String
    nextChar ']' = fail "nothing else to do"
    nextChar '[' = ('[' :) . (++ "]") <$> lift findBalancedBracket
    nextChar x = return [x]

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
compile = evalState (runWithFilter compileChar)
