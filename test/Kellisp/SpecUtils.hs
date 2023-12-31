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
  evalInput defaultEnvRef $ readEval input

-- | Reads input (that may consist of multiple values) and runs
-- the eval monad in the default context
readRunFile :: T.Text -> IO LispVal
readRunFile input = do
  defaultEnvRef <- newIORef defaultEnv
  evalInput defaultEnvRef $ readEvalFile input

-- | Runs a test given some text and the LispVal it should evaluate to
shouldEval :: T.Text -> LispVal -> IO ()
shouldEval input expected = do
  result <- readRun input
  result `shouldBe` expected

-- | Runs a test given some text representing multiple LispValues
-- and the LispVal it should evaluate to
shouldEvalValues :: T.Text -> LispVal -> IO ()
shouldEvalValues input expected = do
  result <- readRunFile input
  result `shouldBe` expected

-- | Runs a test given a file path containing expressions
-- and the result that the file should evaluate to
shouldEvalFile :: FilePath -> LispVal -> IO ()
shouldEvalFile fp expected = do
  defaultEnvRef <- newIORef defaultEnv
  result <- evalInputFile defaultEnvRef fp
  result `shouldBe` expected
