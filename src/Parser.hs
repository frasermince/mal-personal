module Parser
( Parser.read
) where
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad       (void, ap, (>>))
import Types               (Sexp(..))

read :: String -> Either ParseError Sexp
read = parseWithWhitespace sexp

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof (whitespace >> p)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

sexp :: Parser Sexp
sexp = list <|> atom

atom :: Parser Sexp
atom = num <|> symbol

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

num :: Parser Sexp
num = (MalNum . Prelude.read) <$> lexeme(many1 digit)

symbol :: Parser Sexp
symbol = MalSymbol <$> lexeme (many1 $ noneOf " ()")

list :: Parser Sexp
list = MalList <$> (lexeme (char '(')
                 *> many sexp
                 <* lexeme (char ')'))
