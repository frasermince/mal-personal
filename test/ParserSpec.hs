module ParserSpec (spec) where
import Parser
import Test.Hspec
import Control.Exception (evaluate)
import Data.Either
import Types (Sexp(..))

spec :: Spec
spec =
  describe "Parser" $
    describe "read" $ do
      it "parses a number" $
        Parser.read "12" `shouldBe` Right (MalNum 12)

      it "parses a symbol" $
        Parser.read "symbol" `shouldBe` Right (MalSymbol "symbol")

      it "fails to parse a string starting with a number" $
        isLeft (Parser.read "95letters") `shouldBe` True

      it "parses a list" $
        Parser.read "(1 2)" `shouldBe` Right (MalList [MalNum 1, MalNum 2])

      it "parses a nested list" $
        Parser.read "(1 (2 3))" `shouldBe` Right (MalList [MalNum 1, MalList [MalNum 2, MalNum 3]])

      it "fails if parsing a list with parentheses that don't match" $
        isLeft (Parser.read "(1 2 (3)") `shouldBe` True
