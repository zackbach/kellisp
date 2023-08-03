module Kellisp.Environment (defaultEnv) where

import qualified Data.Map as Map

import           Kellisp.Environment.Numeric
import           Kellisp.Types

-- | Default environment in which functions are evaluated
defaultEnv :: Env
-- as more are added, just add them all to the big map (env)
defaultEnv = Map.fromList numericEnv
