module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import qualified Parser
import Evaluator
import Utilities.Types
import Errors

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse Parser.parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled
