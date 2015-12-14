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

      it "allows you to create function" $
        let evaluation = Evaluator.evaluate (MalList [MalList [MalSymbol "fn", MalList [MalSymbol "x"], MalSymbol "x"], MalNum 3], replEnv)
        in  runEval evaluation `shouldBe` Right (MalNum 3)

      it "allows you to handle a nested function" $
        let innerFunction = MalList [MalSymbol "fn", MalList [MalSymbol "x", MalSymbol "y"], MalList [MalSymbol "+", MalSymbol "x", MalSymbol "y"]]
            evaluation = Evaluator.evaluate (MalList [MalList [MalSymbol "fn", MalList [MalSymbol "x"], MalList [innerFunction, MalSymbol "x", MalNum 4]], MalNum 3], replEnv)
        in  runEval evaluation `shouldBe` Right (MalNum 7)

      it "has a function to create a list" $
        let list = MalList [MalSymbol "list", MalNum 4, MalNum 5]
            evaluation = Evaluator.evaluate (list, replEnv)
        in  runEval evaluation `shouldBe` Right (MalList [MalNum 4, MalNum 5])

      it "can see if something is a list" $
        let list = MalList[MalSymbol "list?", MalList [MalSymbol "list", MalNum 4, MalNum 5]]
            evaluation = Evaluator.evaluate (list, replEnv)
        in  runEval evaluation `shouldBe` Right (MalBool "True")

      it "can see if something is not a list" $
        let list = MalList[MalSymbol "list?", MalNum 3]
            evaluation = Evaluator.evaluate (list, replEnv)
        in  runEval evaluation `shouldBe` Right (MalBool "False")


      it "can see if a list is empty" $
        let list = MalList[MalSymbol "empty?", MalList [MalSymbol "list"]]
            evaluation = Evaluator.evaluate (list, replEnv)
        in  runEval evaluation `shouldBe` Right (MalBool "True")


      it "can see if a list is not empty" $
        let list = MalList[MalSymbol "empty?", MalList [MalSymbol "list", MalNum 4, MalNum 5]]
            evaluation = Evaluator.evaluate (list, replEnv)
        in  runEval evaluation `shouldBe` Right (MalBool "False")

