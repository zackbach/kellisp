module Kellisp.Environment (defaultEnv) where

import qualified Data.Map as Map

import           Kellisp.Environment.Boolean
import           Kellisp.Environment.Numeric
import           Kellisp.Environment.Ordered
import           Kellisp.Types

-- | Default environment in which functions are evaluated
defaultEnv :: Env
defaultEnv = Map.fromList $ numericEnv ++ orderedEnv ++ booleanEnv
