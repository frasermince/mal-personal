module Parser
( Parser.read
, Sexp(..)
) where
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad (void, ap, (>>))

read :: String -> Either ParseError Sexp
read = parseWithWhitespace sexp

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof (whitespace >> p)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

data Sexp = Num Integer | Symbol String | List [Sexp]
            deriving (Eq)

instance Show Sexp where
  show (Num x) = show x
  show (Symbol x) = x
  show (List sexps) = "(" ++ foldl convertToString "" sexps ++ ")"
    where
      convertToString "" sexp = show sexp
      convertToString accumulator sexp = accumulator ++ " " ++ show sexp

sexp :: Parser Sexp
sexp = list <|> atom

atom :: Parser Sexp
atom = num <|> symbol

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

num :: Parser Sexp
num = (Num . Prelude.read) <$> lexeme(many1 digit)

symbol :: Parser Sexp
symbol = Symbol <$> lexeme (many1 $ noneOf " ()")

list :: Parser Sexp
list = List <$> (lexeme (char '(')
                 *> many sexp
                 <* lexeme (char ')'))
