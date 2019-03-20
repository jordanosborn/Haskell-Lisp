module Evaluator where
import Utilities.Types

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False

numberp (Number _) = Bool True
numberp _ = Bool False

stringp (String _) = Bool True
stringp _ = Bool False

boolp (Bool _) = Bool True
boolp _ = Bool False

listp (List _) = Bool True
listp (DottedList _ _) = Bool True
listp _ = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

symbol2string, string2symbol :: LispVal -> LispVal
symbol2String (Atom s) = String s
symbol2string _ = String ""
string2symbol (String s) = Atom s
string2symbol _ = Atom ""

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("symbol?", unaryOp symbolp),
    ("string?", unaryOp stringp),
    ("number?", unaryOp numberp),
    ("bool?", unaryOp boolp),
    ("list?", unaryOp listp),
    ("symbol->string", unaryOp symbol2string),
    ("string->symbol", unaryOp string2symbol)]


apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Atom _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
