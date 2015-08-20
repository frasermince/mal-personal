module Rep
( Rep.rep
, Rep.eval
, Rep.print
) where
import Parser
import Text.Parsec

rep :: String -> String
rep = Rep.print . Rep.eval . Parser.read

eval :: Either ParseError Sexp -> Either ParseError Sexp
eval x = x

print :: Either ParseError Sexp -> String
print = either show show
