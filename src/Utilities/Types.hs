module Utilities.Types where
import Data.Complex
import Data.Ratio
import Data.Array

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char
    | Float Double
    | Ratio Rational
    | Complex (Complex Double)
    | Vector (Array Int LispVal)
