module EnvironmentSpec (spec) where
import Test.Hspec
import Types (Sexp(..), Environment(..))
import Environment
import Data.Maybe
import qualified Data.Map as Map

spec :: Spec
spec =
  describe "Environment" $
    describe "get" $ do
      it "searches a flat environment for a symbol" $
        get "Symbol" Environment{outer = Nothing, current = Map.fromList [("Symbol", MalNum 3)]} `shouldBe` Just 3

      it "search a nested environment for a symbol" $
        isJust result `shouldBe` True
        where result = get "+" Environment{outer = Just replEnv, current = currentMap}
              currentMap = Map.fromList [("Symbol", MalNum 3)]
