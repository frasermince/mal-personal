module EnvironmentSpec (spec) where
import Test.Hspec
import Types (Sexp(..), Environment(..))
import Environment
import Data.Maybe
import qualified Data.Map as Map

spec :: Spec
spec =
  describe "Environment" $ do
    describe "get" $ do
      it "finds in a flat environment for a symbol" $
        let command x y = MalNum 3
            currentMap = Map.fromList [("Symbol", command)]
            getCommand = get "Symbol" Environment{outer = Nothing, current = currentMap}
        in  isJust getCommand  `shouldBe` True


      it "finds in a nested environment for a symbol" $
        let command x y = MalNum 3
            currentMap = Map.fromList [("Symbol", command)]
            getCommand = get "+" Environment{outer = Just replEnv, current = currentMap}
        in  isJust getCommand `shouldBe` True

      it "cannot find if not in the set" $
        let command x y = MalNum 3
            currentMap = Map.fromList [("Symbol", command)]
            environment = Environment{outer = Nothing, current = currentMap}
            getCommand = get "Test" environment
        in  isNothing getCommand `shouldBe` True

    describe "set" $
      it "sets the command in the current environment" $
        let command x y = MalNum 3
            currentMap = Map.fromList [("Symbol", command)]
            originalEnvironment = Environment{outer = Nothing, current = currentMap}
            newEnvironment = set "Test" command originalEnvironment
            getCommand = get "Test" newEnvironment
        in  isJust getCommand `shouldBe` True

