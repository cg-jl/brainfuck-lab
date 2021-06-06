{-# LANGUAGE FlexibleContexts #-}

module Syntax (SyntaxError, checkSyntax) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor
import Util

data SourcePos = SourcePos {sourceColumn :: Int, sourceLine :: Int}

columnL :: Lens SourcePos Int
columnL = Lens sourceColumn $ \src col -> src {sourceColumn = col}

lineL :: Lens SourcePos Int
lineL = Lens sourceLine $ \src line -> src {sourceLine = line}

incLine :: SourcePos -> SourcePos
incLine = over lineL (+ 1) . update' columnL 0

incCol :: SourcePos -> SourcePos
incCol = over columnL (+ 1)

instance Show SourcePos where
  show (SourcePos col line) = show line ++ ":" ++ show col

newPos :: SourcePos
newPos = SourcePos 0 1

incPos :: Char -> SourcePos -> SourcePos
incPos '\n' = incLine
incPos _ = incCol

data SyntaxError = MissingLB SourcePos | MissingRB SourcePos

instance Show SyntaxError where
  show (MissingLB pos) = "Missing opening '[' for ']' at " ++ show pos
  show (MissingRB pos) = "Missing closing ']' for '[' at " ++ show pos

data CheckerState = CheckerState {currentPos :: SourcePos, posStack :: [SourcePos]}

posL :: Lens CheckerState SourcePos
posL = Lens currentPos $ \checker pos -> checker {currentPos = pos}

stackL :: Lens CheckerState [SourcePos]
stackL = Lens posStack $ \checker stack -> checker {posStack = stack}

addPosToStack :: CheckerState -> CheckerState
addPosToStack c =
  let pos = view posL c
   in over stackL (pos :) c

incCheckerPos :: Char -> CheckerState -> CheckerState
incCheckerPos = over posL . incPos

incCheckerPos' :: Monad m => Char -> StateT CheckerState m Char
incCheckerPos' c = modify (incCheckerPos c) $> c

missingRight :: [SourcePos] -> StateT CheckerState (Either SyntaxError) a
missingRight = lift . Left . MissingRB . head

missingLeft :: StateT CheckerState (Either SyntaxError) a
missingLeft = gets currentPos >>= lift . Left . MissingLB

tryPopStack :: StateT CheckerState (Either SyntaxError) ()
tryPopStack = do
  stack <- gets posStack
  when (null stack) missingLeft
  modify $ over stackL tail

checkFinalClosing = do
  xs <- gets posStack
  unless (null xs) $ missingRight xs

checkChar :: Char -> StateT CheckerState (Either SyntaxError) ()
checkChar '[' = modify addPosToStack
checkChar ']' = tryPopStack
checkChar _ = return ()

checkString :: String -> StateT CheckerState (Either SyntaxError) ()
checkString xs = mapM_ (incCheckerPos' >=> checkChar) xs >> checkFinalClosing

checkSyntax :: String -> Either SyntaxError String
checkSyntax xs = makeCheck $> xs
  where
    makeCheck = evalStateT (checkString xs) $ CheckerState newPos []


