{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.BooleanSpec where

import           Kellisp.SpecUtils

spec :: Spec
spec = do
  describe "and"
    $ do
      it "ands together two true booleans"
        $ "(and #t #t)" `shouldEval` Bool True
      it "ands together two non-true booleans"
        $ "(and #t #f)" `shouldEval` Bool False
      it "ands together evaluated expressions"
        $ "(and (= 2 2) (= 1 1))" `shouldEval` Bool True
      it "ands together many bools"
        $ "(and #t #t #t #f #t)" `shouldEval` Bool False
      it "ands together one true bool" $ "(and #t)" `shouldEval` Bool True
      it "ands together one false bool" $ "(and #f)" `shouldEval` Bool False
      it "ands together zero bools" $ "(and)" `shouldEval` Bool True

  describe "or"
    $ do
      it "ors together two booleans" $ "(or #t #f)" `shouldEval` Bool True
      it "ors together two false booleans"
        $ "(or #f #f)" `shouldEval` Bool False
      it "ors together evaluated expressions"
        $ "(or (= 1 2) (= 2 2))" `shouldEval` Bool True
      it "ors together many bools"
        $ "(or #f #f #f #t #f)" `shouldEval` Bool True
      it "ors together one true bool" $ "(or #t)" `shouldEval` Bool True
      it "ors together one false bool" $ "(or #f)" `shouldEval` Bool False
      it "ors together zero bools" $ "(or)" `shouldEval` Bool False

