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
