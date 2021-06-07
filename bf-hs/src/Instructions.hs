module Instructions where

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

