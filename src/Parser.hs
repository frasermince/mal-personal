module Parser
( readLang
) where
import Text.Parsec
import Text.Parsec.Char    (char)
import Text.Parsec.String  (Parser)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad       (void, ap, (>>))
import Types               (Sexp(..), MalError(..))

readLang :: String -> Either MalError Sexp
readLang command = either (Left . MalParseError) (Right) (parseWithWhitespace sexp command)

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof (whitespace >> p)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

sexp :: Parser Sexp
sexp = list <|> atom

atom :: Parser Sexp
atom = num <|> bool <|> malString <|> symbol

malString :: Parser Sexp
malString = MalString <$> (char '"' *> stringChars <* lexeme (char '"'))

stringChars = many $ escapedChars <|> noneOf "\\\""

-- I took this function from the existing example. One of the only things I've copied in this project.
escapedChars = do char '\\'
                  x <- oneOf "\\\"n"
                  case x of
                    'n' -> return '\n'
                    _   -> return x


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

bool :: Parser Sexp
bool = MalBool <$> (==) "true" <$> (lexeme $ (string "true") <|> (try $ string "false"))

num :: Parser Sexp
num = (MalNum . Prelude.read) <$> lexeme(many1 digit)

symbol :: Parser Sexp
symbol = MalSymbol <$> lexeme (many1 $ noneOf " ()")

list :: Parser Sexp
list = MalList <$> (lexeme (char '(')
                 *> many sexp
                 <* lexeme (char ')'))
