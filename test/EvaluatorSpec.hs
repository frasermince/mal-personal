module EvaluatorSpec (spec) where
import Evaluator
import Test.Hspec
import Types (Sexp(..), Environment(..), runEval, runEvalForTuple)
import Control.Monad.Writer.Lazy
import Environment
import Data.Either

spec :: Spec
spec =
  describe "Evaluator" $
    describe "eval" $ do
      it "solves simple equations" $
        let evaluation = Evaluator.evaluate ((MalList [MalSymbol "+", MalNum 2, MalNum 1]), replEnv)
        in runEval evaluation `shouldBe` Right (MalNum 3)

      it "solves more complicated equations" $
        let firstSublist = MalList [MalSymbol "+", MalNum 2, MalNum 1]
            secondSublist = MalList [MalSymbol "*", MalNum 2, MalNum 3]
            evaluation = Evaluator.evaluate ((MalList [MalSymbol "-", firstSublist, secondSublist]), replEnv)
        in  runEval evaluation `shouldBe` Right (MalNum (-3))

      it "evaluates defs" $
        let evaluation = Evaluator.evaluate (MalList [MalSymbol "def", MalSymbol "x", MalNum 3], replEnv)
        in  runEval evaluation `shouldBe` Right (MalNum 3)

      it "remembers defs between lists" $
        let firstEval = Evaluator.evaluate (MalList [MalSymbol "def", MalSymbol "x", MalNum 3], replEnv)
            newEnv = case (runEvalForTuple firstEval) of
              Right (s, e) -> e
              Left  error -> replEnv
            secondEval = Evaluator.evaluate (MalSymbol "x", newEnv)
        in  runEval secondEval `shouldBe` Right (MalNum 3)

      it "Can evalute a let expression" $
         let evaluation = Evaluator.evaluate (MalList [MalSymbol "let", MalList [MalSymbol "x", MalNum 3], MalSymbol "x"], replEnv)
         in  runEval evaluation `shouldBe` Right (MalNum 3)

      it "Removes let environment afterwards" $
         let firstEval = Evaluator.evaluate (MalList [MalSymbol "let", MalList [MalSymbol "x", MalNum 3], MalSymbol "x"], replEnv)
             newEnv = case (runEvalForTuple firstEval) of
               Right (s, e) -> e
             secondEval = Evaluator.evaluate (MalSymbol "x", newEnv)
         in  isLeft (runEval secondEval) `shouldBe` True

      it "Fails if an odd number of list elements are present in the let" $
         let evaluation = Evaluator.evaluate (MalList [MalSymbol "let", MalList [MalSymbol "x"], MalSymbol "x"], replEnv)
         in  isLeft (runEval evaluation) `shouldBe` True
