module Main where
import Control.Monad
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readOct, readHex)

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (head args))

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many parseChar
    char '"'
    return $ String x

parseChar :: Parser Char
parseChar = noneOf "\"" <|> parseEscape

parseEscape :: Parser Char
parseEscape = char '\\' >> choice
    [ char 'n' >> return '\n'
    , char 'r' >> return '\r'
    , char 't' >> return '\t'
    , char '\\' >> return '\\'
    , anyChar
    ]

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseOctal <|> parseHexadecimal

parseDecimal :: Parser LispVal
parseDecimal = do
    digits <- many1 digit
    return $ case reads digits of
        [(num, "")] -> Number num
        _ -> error "Invalid decimal number"

parseOctal :: Parser LispVal
parseOctal = do
    char '0'
    digits <- many1 octDigit
    return (Number (fst (head (readOct digits))))

parseHexadecimal :: Parser LispVal
parseHexadecimal = do
    try (string "#x")
    digits <- many1 hexDigit
    return (Number (fst (head (readHex digits))))

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

