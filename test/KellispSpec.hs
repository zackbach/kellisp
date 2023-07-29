module KellispSpec where

import Test.Hspec
import Kellisp

spec :: Spec
spec = do
  describe "test" $ do
    it "does some stuff" $ do
      2 + 2 `shouldBe` 4
