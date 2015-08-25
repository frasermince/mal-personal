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
        let command x y = MalNum 3
            currentMap = Map.fromList [("Symbol", command)]
            getCommand = get "Symbol" Environment{outer = Nothing, current = currentMap}
        in  isJust getCommand  `shouldBe` True


      it "search a nested environment for a symbol" $
        let command x y = MalNum 3
            currentMap = Map.fromList [("Symbol", command)]
            getCommand = get "+" Environment{outer = Just replEnv, current = currentMap}
        in  isJust getCommand `shouldBe` True

