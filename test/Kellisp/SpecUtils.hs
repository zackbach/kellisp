module Kellisp.SpecUtils
    ( module Kellisp.SpecUtils
    , module Kellisp.Types
    , module Test.Hspec) where

import           Data.IORef
import qualified Data.Text as T

import           Kellisp
import           Kellisp.Environment
import           Kellisp.Types

import           Test.Hspec

-- | Reads input and runs the eval monad in the default context
readRun :: T.Text -> IO LispVal
readRun input = do
  defaultEnvRef <- newIORef defaultEnv
  evalInput defaultEnvRef input

-- | Runs a test given some text and the LispVal it should evaluate to
shouldEval :: T.Text -> LispVal -> IO ()
shouldEval input expected = do
  result <- readRun input
  result `shouldBe` expected
