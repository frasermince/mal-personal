module EnvironmentSpec (spec) where
import Test.Hspec
import Types (Sexp(..), Environment(..), runEval)
import Environment
import Core
import Data.Maybe
import qualified Data.Map as Map

spec :: Spec
spec =
  describe "Environment" $ do
    describe "startingEnv" $
      it "finds the + and applies it" $
        let possibleCommand = case (get "+" startingEnv) of
                               (Just (MalFunction possibleCommand)) -> Just possibleCommand
            command = fromMaybe (\bindings env -> return $ MalNum 0) possibleCommand
            eval = command [MalNum 1, MalNum 2] startingEnv
        in  runEval eval `shouldBe` (Right $ MalNum 3)
    describe "get" $ do
      it "finds in a flat environment for a symbol" $
        let command = MalNum 3
            currentMap = Map.fromList [("Symbol", command)]
            getCommand = get "Symbol" [currentMap]
        in  isJust getCommand  `shouldBe` True


      it "finds in a nested environment for a symbol" $
        let command = MalNum 3
            currentMap = Map.fromList [("Symbol", command)]
            base = head startingEnv
            getCommand = get "+" [currentMap, base]
        in  isJust getCommand `shouldBe` True

      it "cannot find if not in the set" $
        let command = MalNum 3
            currentMap = Map.fromList [("Symbol", command)]
            environment = [currentMap]
            getCommand = get "Test" environment
        in  isNothing getCommand `shouldBe` True

    describe "set" $ do
      it "sets the command in the current environment" $
        let command = MalNum 3
            currentMap = Map.fromList [("Symbol", command)]
            originalEnvironment = [currentMap]
            newEnvironment = set "Test" command originalEnvironment
            getCommand = get "Test" newEnvironment
        in  getCommand `shouldBe` (Just $ MalNum 3)

      it "can override the previous value" $
         let command = MalNum 3
             currentMap = Map.fromList [("Symbol", command)]
             originalEnvironment = [currentMap]
             newEnvironment = set "Test" command originalEnvironment
             finalEnvironment = set "Test" (MalNum 4) newEnvironment
             getCommand = get "Test" finalEnvironment
         in  getCommand `shouldBe` (Just $ MalNum 4)

