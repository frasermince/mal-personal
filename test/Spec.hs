import Rep
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $
  describe "Rep" $
    describe "Read" $
      it "parses the data" $
        Rep.read "(+ 1 2)" `shouldBe` List(Atom(+) Atom 1 Atom 2)


