module Kellisp.SpecUtils where

import           Control.Monad.Reader

import qualified Data.Text as T

import           Kellisp
import           Kellisp.Environment
import           Kellisp.Types

import           Test.Hspec

-- | Reads input and runs the eval monad in the default context
readRun :: T.Text -> IO LispVal
readRun input = runReaderT (unEval $ readEval input) defaultEnv

-- | Runs a test given some test and the LispVal it should evaluate to
shouldEval :: T.Text -> LispVal -> IO ()
shouldEval input expected = do
  result <- readRun input
  result `shouldBe` expected
