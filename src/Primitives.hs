{-# LANGUAGE ExistentialQuantification #-}
module Primitives where
import Utilities.Types
import Errors
import Utilities.Tools
import Control.Monad.Except

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = Right $ f v
unaryOp f x = throwError $ NumArgs 1 x

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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 then throwError $ NumArgs 2 args
    else do
        left <- unpacker $ head args
        right <- unpacker $ args !! 1
        return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _ = String ""
string2symbol (String s) = Atom s
string2symbol _ = Atom ""

car :: [LispVal] -> ThrowsError LispVal
car value = case value of
    [List (x : xs)] -> return x
    [DottedList (x : xs) _] -> return x
    [badArg] -> throwError $ TypeMismatch "pair" badArg
    badArgList -> throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr value = case value of
    [List (x : xs)] -> return $ List xs
    [DottedList [_] x] -> return x
    [DottedList (_ : xs) x] -> return $ DottedList xs x
    [badArg] -> throwError $ TypeMismatch "pair" badArg
    badArgList -> throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons value = case value of
    [x1, List []] -> return $ List [x1]
    [x, List xs] -> return $ List $ x : xs
    [x, DottedList xs xlast] -> return $ DottedList (x : xs) xlast
    [x1, x2] -> return $ DottedList [x1] x2
    badArgList -> throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv value = case value of
    [Bool arg1, Bool arg2] -> return $ Bool $ arg1 == arg2
    [Number arg1, Number arg2] -> return $ Bool $ arg1 == arg2
    [String arg1, String arg2] -> return $ Bool $ arg1 == arg2
    [Atom arg1, Atom arg2] -> return $ Bool $ arg1 == arg2
    [DottedList xs x, DottedList ys y] -> eqv [List $ xs ++ [x], List $ ys ++ [y]]
    [List arg1, List arg2] -> return $ Bool $ (length arg1 == length arg2) &&
        (all eqvPair $ zip arg1 arg2) where
            eqvPair (x1, x2) = case eqv [x1, x2] of
                Left err -> False
                Right (Bool val) -> val
    [_, _] -> return $ Bool False
    badArgList -> throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal value = case value of
    [arg1, arg2] -> do
        primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
            [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
        eqvEquals <- eqv [arg1, arg2]
        return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
    badArgList -> throwError $ NumArgs 2 badArgList

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
    ("string->symbol", unaryOp string2symbol),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)]