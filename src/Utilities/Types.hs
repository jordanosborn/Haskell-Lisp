module Utilities.Types where
import Data.Complex
import Data.Ratio
import Data.Array
import Data.IORef
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

type Env = IORef [(String, IORef LispVal)]
type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

data LispError = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

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
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func {params :: [String], vararg :: Maybe String,
        body :: [LispVal], closure :: Env}


showVal :: LispVal -> String
showVal val = case val of
    String contents -> "\"" ++ contents ++ "\""
    Atom name -> name
    Number contents -> show contents
    Bool True -> "#t"
    Bool False -> "#f"
    List contents -> "(" ++ unwordsList contents ++ ")"
    DottedList head tail -> "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
    PrimitiveFunc _ -> "<primitive"
    Func {params = args, vararg = varargs, body = body, closure = env} ->
        "(lambda (" ++ unwords (map show args) ++
            (case varargs of
                Nothing -> ""
                Just arg -> " . " ++ arg) ++ ") ...)"


instance Show LispVal where show = showVal
instance Eq LispVal where (==) = (==)


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal