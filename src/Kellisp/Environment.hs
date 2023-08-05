module Kellisp.Environment (defaultEnv) where

import qualified Data.Map as Map

import           Kellisp.Environment.Boolean
import           Kellisp.Environment.Numeric
import           Kellisp.Environment.Ordered
import           Kellisp.Environment.TypePredicates
import           Kellisp.Types

-- | Default environment in which functions are evaluated
defaultEnv :: Env
-- TODO: determine if there is a faster way to do this (likely unimportant)
defaultEnv =
  Map.fromList $ numericEnv ++ orderedEnv ++ booleanEnv ++ predicateEnv
