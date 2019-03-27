module Main where
import           System.Environment
import           Text.ParserCombinators.Parsec
import           Control.Monad.Except
import           System.IO
import qualified Parser
import           Evaluator
import           Utilities.Types
import           Errors
import           Data.IORef
import           Data.Maybe
import           Control.Applicative

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings
        >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (show <$> eval env (List [Atom "load", String (head args)]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl =
    primitiveBindings
        >>= until_ (== "quit") (readPrompt "Lisp> ")
        .   evalAndPrint

main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl else runOne args
