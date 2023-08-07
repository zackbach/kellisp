module Kellisp.Environment.TypeConversion (conversionEnv) where

import Kellisp.Environment.PrimUtils

conversionEnv :: [(Text, LispVal)]
conversionEnv = []

{-
(possible move to numeric?)
number->string
string->number

symbol->string
string->symbol

Note: no string->list conversion because no characters implemented

read and write procedures?
-}
