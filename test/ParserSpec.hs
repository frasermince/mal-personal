module ParserSpec (spec) where
import Parser (readLang)
import Test.Hspec
import Control.Exception (evaluate)
import Core
import Data.Either
import Types (Sexp(..))

spec :: Spec
spec =
  describe "Parser" $
    describe "readLang" $ do
      it "parses a number" $
        readLang "12" `shouldBe` Right (MalNum 12)

      it "parses a symbol" $
        readLang "symbol" `shouldBe` Right (MalSymbol "symbol")

      it "fails to parse a string starting with a number" $
        isLeft (readLang "95letters") `shouldBe` True

      it "parses a list" $
        readLang "(1 2)" `shouldBe` Right (MalList [MalNum 1, MalNum 2])

      it "parses a nested list" $
        readLang "(1 (2 3))" `shouldBe` Right (MalList [MalNum 1, MalList [MalNum 2, MalNum 3]])

      it "fails if parsing a list with parentheses that don't match" $
        isLeft (readLang "(1 2 (3)") `shouldBe` True

      it "parses a string" $
        readLang "\"Hello\"" `shouldBe` Right (MalString "Hello")

      it "parses a string with whitespace" $
        readLang "\" Hi\"" `shouldBe` Right (MalString " Hi")

      it "fails to parse a string with mismatched quotes" $
        isLeft (readLang "\"Hello") `shouldBe` True

      it "parses a string with escaped quotes" $
        readLang "\"Hi\"\"" `shouldBe` Right (MalString "Hi\"")
