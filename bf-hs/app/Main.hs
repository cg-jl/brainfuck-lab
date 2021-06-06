module Main (main) where

import Lib
import System.Environment (getArgs)

parseArgs :: IO (Either String String)
parseArgs = do
  args <- getArgs
  return $
    if null args
      then Left "At least one argument with the file name to run is required."
      else return $ head args


main :: IO ()
main = parseArgs >>= either handleError runFile
