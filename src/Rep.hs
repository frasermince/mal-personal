module Rep
( Rep.rep
, Rep.eval
, Rep.print
) where
import Read
import Text.Parsec

rep :: String -> String
rep = Rep.print . Read.read

eval :: Sexp -> Sexp
eval x = x

print :: Either ParseError Sexp -> String
print = either show show
