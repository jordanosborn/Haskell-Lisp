module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Utilities.Number
import Numeric
import Data.Ratio
import Data.Complex
import Utilities.Types

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do
    char '\\' -- a backslash
    x <- oneOf "\\\"nrt" -- return either backslash or doublequote
    return $ case x of
        '\\' -> x
        '"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space")
        <|> do { x <- anyChar; notFollowedBy alphaNum; return [x]}
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        _ -> head value

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseDecimal1 :: Parser LispVal
parseDecimal1 = Number . read <$> many1 digit

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
    try $ string "#d"
    x <- many1 digit
    (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 octDigit
    return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
    try $ string "#o"
    x <- many1 (oneOf "10")
    return $ Number (bin2dig x)

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float (fst . head $ readFloat (x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio (read x % read y)

parseComplex :: Parser LispVal
parseComplex = do
    x <- try parseFloat <|> parseDecimal1 <|> parseDecimal2
    char '+'
    y <- try parseFloat <|> parseDecimal1 <|> parseDecimal2
    char 'i'
    return $ Complex (toDouble x :+ toDouble y)

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseRatio
    <|> try parseNumber
    <|> try parseBool
    <|> try parseCharacter

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found Value"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn  $ readExpr expr