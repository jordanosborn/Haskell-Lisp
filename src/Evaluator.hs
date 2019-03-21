module Evaluator where
import Utilities.Types
import Errors
import Utilities.Tools
import Control.Monad.Except
import Primitives


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction
    "Unrecognised primitive function args" func)
    ($ args) (lookup func primitives)

cond :: [LispVal] -> ThrowsError LispVal
cond ((List (Atom "else" : value : [])) : []) = eval value
cond ((List (condition : value : [])) : alts) = do
    result <- eval condition
    boolResult <- unpackBool result
    if boolResult then eval value
    else cond alts
cond ((List a) : _) = throwError $ NumArgs 2 a
cond (a : _) = throwError $ NumArgs 2 [a]
cond _ = throwError $ Default "Not viable alternative in cond"

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List ((Atom "cond") : alts)) = cond alts
eval form@(List (Atom "case" : key : clauses)) =
    if null clauses then throwError $ BadSpecialForm "no true clause in case expression: " form
    else case head clauses of
        List (Atom "else" : exprs) -> mapM eval exprs >>= return . last
        List ((List datums) : exprs) -> do
            result <- eval key
            equality <- mapM (\x -> eqv [result, x]) datums
            if Bool True `elem` equality then
                mapM eval exprs >>= return . last
            else eval $ List (Atom "case" : key : tail clauses)
        _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        Bool True -> eval conseq
        _ -> throwError $ TypeMismatch "bool" pred
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm
