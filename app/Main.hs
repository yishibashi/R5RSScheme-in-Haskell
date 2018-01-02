module Main where

import           Control.Monad
import           Data.Complex
import           Data.Ratio
import           Lib
import           Numeric
import           Prelude
import           System.Environment
-- import           Text.Parsec        hiding (spaces)
import           Text.ParserCombinators.Parsec hiding (spaces)


{-- Scheme data types --}
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
--             deriving Show

instance Show LispVal where
    show = showVal

{-- +-+-+-+-+-+-+-+-+-+-+ <Evaluator +-+-+-+-+-+-+-+-+-+-+ --}

showVal :: LispVal -> String
showVal (String contents)  = "\"" ++ contents ++ "\""
showVal (Atom name)        = name
showVal (Number contents) = show contents
{-
showVal (Float contents) = show contents
showVal (Ratio contents) = show contents
showVal (Complex contents) = show contents
-}
showVal (Bool True)        = "#t"
showVal (Bool False)       = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Number _)             = val
{-
eval val@(Float _)              = val
eval val@(Ratio _)              = val
eval val@(Complex _)            = val
-}
eval val@(Bool _)               = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum (Number n) = n
{--
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                            then 0
                            else fst $ parsed !! 0
                            --}
-- unpackNum (List [n]) = unpackNum n
unpackNum _          = 0


unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp op [v] = op v


symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _        = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
tringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _) = Bool True
boolp   _        = Bool False
listp   (List _)         = Bool True
listp   (DottedList _ _) = Bool True
listp   _                = Bool False
symbol2string :: LispVal -> LispVal
symbol2string (Atom s) = (String s)
symbol2string _        = String ""
string2symbol :: LispVal -> LispVal
string2symbol (String a) = (Atom a)
string2symbol _          = Atom ""



{-- +-+-+-+-+-+-+-+-+-+-+ Evaluator> +-+-+-+-+-+-+-+-+-+-+ --}

{-- +-+-+-+-+-+-+-+-+-+-+ <Parser +-+-+-+-+-+-+-+-+-+-+ --}
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseString :: Parser LispVal
parseString = do
    char '"'
    -- s <- many $ parseEsc <|> noneOf "\"\\"
    s <- many $ parseEsc <|> noneOf ['"','\\']
    char '"'
    return $ String s

parseEsc :: Parser Char
parseEsc = do
    char '\\'
    -- e <- oneOf "\\\"nrt"
    e <- oneOf ['\\','"','n','r','t']
    return $ case e of
               '\\' -> e
               '"'  -> e
               'n'  -> '\n'
               'r'  -> '\r'
               't'  -> '\t'

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    x <- try (string "newline" <|> string "space")
         <|> do { c <- anyChar; notFollowedBy alphaNum ; return [c] }
    return $ Character $ case x of
                         "space"   -> ' '
                         "newline" -> '\n'
                         otherwise -> (x !! 0)

parseNumber :: Parser LispVal
parseNumber = parseDec1 <|> parseDec2 <|> {-- parseBin <|>--} parseOct <|> parseHex

parseDec1 :: Parser LispVal
parseDec1 = (many1 digit) >>= (return . Number . read)

parseDec2 :: Parser LispVal
parseDec2 = do
    try $ string "#d"
    x <- many1 digit
    return $ (Number . read) x
{--
parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    x <- many1 (oneOf "01")
    return $ Number $ fst $ (readBin x) !! 0
--}
parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 octDigit
    return $ Number $ fst $ (readOct x) !! 0

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number $ fst $ (readHex x) !! 0


{--
 - Exercise
parseNumber = do
    digits <- many1 digit
    return $ (Number . read) digits

-- parseNumber = (many1 digit) >>= (\x -> return ((Number . read) x))
--}

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float $ fst . head $ readFloat (x ++ "." ++ y)

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio $ (read x) % (read y)

toDouble :: LispVal -> Double
toDouble (Float f)  = realToFrac f
toDouble (Number n) =  fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do
    x <- (try parseFloat <|> parseDec1)
    char '+'
    y <- (try parseFloat <|> parseDec1)
    char 'i'
    return $ Complex (toDouble x :+ toDouble y)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQQuote :: Parser LispVal
parseQQuote = do
    char '`'
    x <- parseExpr
    return $ List [Atom "qquote", x]

parseUQuote :: Parser LispVal
parseUQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "uquote", x]



parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> try parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> parseQuoted
        <|> parseQQuote
        <|> parseUQuote
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

{-- +-+-+-+-+-+-+-+-+-+-+ Parser> +-+-+-+-+-+-+-+-+-+-+ --}


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> String $ "No match: " ++ show err
    Right val -> val


main :: IO ()
main = getArgs >>= print . eval . readExpr . head

    {-- Exercises --}
