module EvaluatorSpec (spec) where
import Evaluator
import Test.Hspec
import Types (Sexp(..))

spec :: Spec
spec =
  describe "Evaluator" $
    describe "eval" $
      it "solves simple calculations" $
        Evaluator.evaluate(MalList[MalSymbol "+", MalNum 2, MalNum 1]) `shouldBe` Right (MalNum 3)
