module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import qualified Parser

readExpr :: String -> String
readExpr input = case parse Parser.parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found Value"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn  $ readExpr expr