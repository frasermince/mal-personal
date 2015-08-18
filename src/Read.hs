module Read
( Read.read
, Sexp(..)
) where
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad (void, ap, (>>))

read :: String -> Either ParseError Sexp
read = parseWithWhitespace sexp

data Sexp = Num Integer | Symbol String
            deriving (Eq,Show)

sexp :: Parser Sexp
sexp = atom

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof (whitespace >> p)

atom :: Parser Sexp
atom = num <|> symbol 

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

num :: Parser Sexp
num = (Num . Prelude.read) <$> lexeme(many1 digit)

symbol :: Parser Sexp
symbol = Symbol <$> whole
  where
    whole = lexeme ((:) <$> firstChar <*> many rest)
    firstChar = letter <|> char '_'
    rest = digit <|> firstChar

