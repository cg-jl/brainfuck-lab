{-# LANGUAGE TupleSections #-}

module Lib (runFile, handleError) where

import Compile
import Control.Monad
import Interpreter
import Syntax
import System.IO (hPutStrLn, stderr)

handleError :: Show a => a -> IO ()
handleError = hPutStrLn stderr . ("Error: " ++) . show

runFile :: String -> IO ()
runFile = readFile >=> either handleError (run . compile) . checkSyntax
