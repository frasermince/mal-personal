module EvaluatorSpec (spec) where
import Evaluator
import Test.Hspec
import Types (Sexp(..), Environment(..), runEval)
import Environment

spec :: Spec
spec =
  describe "Evaluator" $
    describe "eval" $
      it "solves simple calculations" $
        let evaluation = Evaluator.evaluate (MalList [MalSymbol "+", MalNum 2, MalNum 1]) replEnv
        in  runEval evaluation `shouldBe` Right (MalNum 3)
