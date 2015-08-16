module Rep
( Rep.rep
, Rep.read
, Rep.eval
, Rep.print
) where

rep :: String -> String
rep = Rep.print . Rep.eval . Rep.read

read :: String -> String
read x = x

eval :: String -> String
eval x = x

print :: String -> String
print x = x
