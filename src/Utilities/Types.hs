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
    |


showVal :: LispVal -> String
showVal val = case val of
    String contents -> "\"" ++ contents ++ "\""
    Atom name -> name
    Number contents -> show contents
    Bool True -> "#t"
    Bool False -> "#f"
    List contents -> "(" ++ unwordsList contents ++ ")"
    DottedList head tail -> "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal
instance Eq LispVal where (==) = (==)


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal