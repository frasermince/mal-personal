import Parser
import Test.Hspec
import Control.Exception (evaluate)
import Data.Either

main :: IO ()
main = hspec $
  describe "Parser" $
    describe "read" $ do
      it "parses a number" $
        Parser.read "12" `shouldBe` Right (Num 12)

      it "parses a symbol" $
        Parser.read "symbol" `shouldBe` Right (Symbol "symbol")

      it "fails to parse a string starting with a number" $
        isLeft (Parser.read "95letters") `shouldBe` True

      it "parses a list" $
        Parser.read "(1 2)" `shouldBe` Right (List [Num 1, Num 2])

      it "parses a nested list" $
        Parser.read "(1 (2 3))" `shouldBe` Right (List [Num 1, List [Num 2, Num 3]])

      it "fails if parsing a list with parentheses that don't match" $
        isLeft (Parser.read "(1 2 (3)") `shouldBe` True
