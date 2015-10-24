module EvaluatorSpec (spec) where
import Evaluator
import Test.Hspec
import Types (Sexp(..), Environment(..), runEval, runEvalForTuple)
import Control.Monad.Writer.Lazy
import Core
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

      it "can override a def" $
        let firstEval = Evaluator.evaluate (MalList [MalSymbol "def", MalSymbol "x", MalNum 3], replEnv)
            newEnv = case (runEvalForTuple firstEval) of
              Right (s, e) -> e
              Left  error -> replEnv
            secondEval = Evaluator.evaluate (MalList [MalSymbol "def", MalSymbol "x", MalNum 4], replEnv)
        in  runEval secondEval `shouldBe` Right (MalNum 4)


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

      it "returns last expression from do expressions" $
         let evaluation = Evaluator.evaluate (MalList [MalSymbol "do", MalList [MalSymbol "def", MalSymbol "x", MalNum 3], MalNum 2], replEnv)
         in  runEval evaluation `shouldBe` Right (MalNum 2)

      it "executes every value from a do expression" $
         let evaluation = Evaluator.evaluate (MalList [MalSymbol "do", MalList [MalSymbol "def", MalSymbol "x", MalNum 3], MalNum 2], replEnv)
             sideEffect = do (_, env) <- listen evaluation
                             Evaluator.evaluate (MalSymbol "x", env)
         in  runEval sideEffect `shouldBe` Right (MalNum 3)

      it "evaluates if statements true case" $
         let evaluation = Evaluator.evaluate (MalList [MalSymbol "if", MalBool "true", MalNum 3, MalNum 2], replEnv)
         in  runEval evaluation `shouldBe` Right (MalNum 3)

      it "evaluates if statements false case" $
         let evaluation = Evaluator.evaluate (MalList [MalSymbol "if", MalBool "false", MalNum 3, MalNum 2], replEnv)
         in  runEval evaluation `shouldBe` Right (MalNum 2)
