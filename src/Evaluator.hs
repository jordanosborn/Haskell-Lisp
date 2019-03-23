module Evaluator where
import Utilities.Types
import Errors
import Utilities.Tools
import Control.Monad.Except
import Primitives
import Data.IORef
import Data.Maybe


type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . flip writeIORef value)
        (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where
        extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do
            ref <- newIORef value
            return (var, ref)


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction
    "Unrecognised primitive function args" func)
    ($ args) (lookup func primitives)

cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env [List [Atom "else", value]] = eval env value
cond env (List [condition, value] : alts) = do
    result <- eval env condition
    boolResult <- liftThrows (unpackBool result)
    if boolResult then eval env value
    else cond env alts
cond env (List a : _) = throwError $ NumArgs 2 a
cond env (a : _) = throwError $ NumArgs 2 [a]
cond env _ = throwError $ Default "Not viable alternative in cond"

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List (Atom "cond" : alts)) = cond env alts
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env form@(List (Atom "case" : key : clauses)) =
    if null clauses then throwError $ BadSpecialForm "no true clause in case expression: " form
    else case head clauses of
        List (Atom "else" : exprs) -> last <$> mapM (eval env) exprs
        List (List datums : exprs) -> do
            result <- eval env key
            equality <- liftThrows (mapM (\x -> eqv [result, x]) datums)
            if Bool True `elem` equality then
                last <$> mapM (eval env) exprs
            else eval env $ List (Atom "case" : key : tail clauses)
        _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        Bool True -> eval env conseq
        _ -> throwError $ TypeMismatch "bool" pred
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm
