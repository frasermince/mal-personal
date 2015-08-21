module Types
( Sexp(..)
) where

data Sexp = MalNum Integer | MalSymbol String | MalList [Sexp]
            deriving (Eq)

instance Show Sexp where
  show (MalNum x) = show x
  show (MalSymbol x) = x
  show (MalList sexps) = "(" ++ foldl convertToString "" sexps ++ ")"
    where
      convertToString "" sexp = show sexp
      convertToString accumulator sexp = accumulator ++ " " ++ show sexp
