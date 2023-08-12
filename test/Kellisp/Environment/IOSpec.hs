{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.IOSpec where

import           Kellisp.SpecUtils

import           System.IO.Error (isDoesNotExistError)

spec :: Spec
spec = do
  describe "slurp"
    $ do
      it "can read files"
        $ "(slurp \"test/ksp/short.ksp\")" `shouldEval` String "(+ 1 2)"
      it "throws appropriate errors"
        $ readRun "(slurp \"test/ksp/nonexistant.ksp\")"
        `shouldThrow` isDoesNotExistError

  describe "read"
    $ do
      it "can parse values and doesn't evaluate"
        $ "(read \"(+ 1 2)\")"
        `shouldEval` List [Atom "+", Integer 1, Integer 2]
      it "propagates parse errors"
        -- anyException for here temporarily bc idk how ErrorBundle works
        $ readRun "(read \"(1 2\")" `shouldThrow` anyException
