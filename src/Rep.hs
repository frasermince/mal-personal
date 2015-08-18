module Rep
( Rep.rep
, Rep.eval
, Rep.print
) where
import Read

rep :: String -> String
rep = Rep.print . Rep.eval . Read.read


eval :: String -> String
eval x = x

print :: String -> String
print x = x
