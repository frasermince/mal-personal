import Read
import Test.Hspec
import Control.Exception (evaluate)
import Data.Either

main :: IO ()
main = hspec $
  describe "Read" $
    describe "read" $ do
      it "parses a number" $
        Read.read "12" `shouldBe` Right (Num 12)

      it "parses a symbol" $
        Read.read "symbol" `shouldBe` Right (Symbol "symbol")

      it "fails to parse a string starting with a number" $
        isLeft (Read.read "95letters") `shouldBe` True

      it "parses a list" $
        Read.read "(1 2)" `shouldBe` Right (List [Num 1, Num 2])

      it "parses a nested list" $
        Read.read "(1 (2 3))" `shouldBe` Right (List [Num 1, List [Num 2, Num 3]])

      it "fails if parsing a list with parentheses that don't match" $
        isLeft (Read.read "(1 2 (3)") `shouldBe` True
