module Errors where
import Utilities.Types
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ":" ++ varname
showError (BadSpecialForm message form) = message ++ ":" ++ show form
showError (NotFunction message func) = message ++ ":" ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
    ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
    ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default defaultErr) = "Unknown error: " ++ defaultErr
 
instance Show LispError where show = showError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)
