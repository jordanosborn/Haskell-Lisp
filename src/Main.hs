module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import qualified Parser
import Evaluator
import Utilities.Types

readExpr :: String -> LispVal
readExpr input = case parse Parser.parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head