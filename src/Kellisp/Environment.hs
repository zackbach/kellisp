module Kellisp.Environment (defaultEnv) where

import qualified Data.Map as Map

import           Kellisp.Environment.Boolean
import           Kellisp.Environment.IO
import           Kellisp.Environment.List
import           Kellisp.Environment.Numeric
import           Kellisp.Environment.Ordered
import           Kellisp.Environment.String
import           Kellisp.Environment.TypeConversion
import           Kellisp.Environment.TypePredicates
import           Kellisp.Types

-- | Default environment in which functions are evaluated
defaultEnv :: Env
defaultEnv = Map.fromList
  $ numericEnv
  ++ orderedEnv
  ++ booleanEnv
  ++ predicateEnv
  ++ listEnv
  ++ stringEnv
  ++ conversionEnv
  ++ ioEnv
