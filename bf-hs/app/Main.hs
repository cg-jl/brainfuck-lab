module Main where

import Lib (runFile)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Control.Monad

parseArgs :: IO (Either String String)
parseArgs = do
  args <- getArgs
  return $
    if null args
      then Left "At least one argument with the file name to run is required."
      else Right . head $ args

handleError :: String -> IO ()
handleError = hPutStrLn stderr . ("Error: " ++)

main :: IO ()
main = parseArgs >>= either handleError runFile
