import Read
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $
  describe "Rep" $
    describe "Read" $
      it "parses the data" $
        Read.read "2" `shouldBe` Right (Num 2)


