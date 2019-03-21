module Utilities.Tools where
import Utilities.Types
import Control.Monad.Except
import Errors

unpackNum :: LispVal -> ThrowsError Integer
unpackNum value = case value of
    Number n -> return n
    String n -> let parsed = reads n in
        if null parsed then
            throwError $ TypeMismatch "number" $ String n
        else
            return $ fst $ (head parsed)
    List [n] -> unpackNum n
    notNum -> throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr value = case value of
    String s -> return s
    Number s -> return $ show s
    Bool s  -> return $ show s
    notString -> throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool value = case value of
    Bool b -> return b
    notBool -> throwError $ TypeMismatch "boolean" notBool

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [List arg1, List arg2] = return $ Bool $
    (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2) where
        eqvPair (x1, x2) = case eqvFunc [x1, x2] of
            Left err -> False
            Right (Bool val) -> val
