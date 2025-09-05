module Parser (parseExpr) where

import Control.Applicative
import Data.Char
    ( isSpace
    , isAlpha
    , isDigit
    )
import Data.List (nub)
import Data.Maybe (isJust)
import Lang

type Location = Int

data Input = Input
    { inputLoc :: Location
    , inputStr :: String
    } deriving (Show, Eq)

get :: Input -> (Input, Maybe Char)
get i@(Input _ "") = (i, Nothing)
get (Input loc (x:xs)) = (Input (loc + 1) xs, Just x)

newtype Parser a = Parser { runParser :: Input -> Either [ParserError] (Input, a) }

data ParserError
    = EndOfInput Location
    | ExpectedEndOfInput Location Char
    | UnexpectedInput String String
    | ReservedKeyword String
    | UnparsedInput Location String
    | EmptyError
    deriving Eq

instance Show ParserError where
    show (EndOfInput i) = "Reached end of the input at " ++ show i
    show (ExpectedEndOfInput i c) = "Expected end of the input at " ++ show i ++ " but got character `" ++ [c] ++ "`"
    show (UnexpectedInput expected result) = "Expected to get " ++ expected ++ " but got " ++ result
    show (ReservedKeyword keyword) = "`" ++ keyword ++ "` is a reserved keyword"
    show (UnparsedInput i s) = "Unparsed input at " ++ show i ++ ": `" ++ s ++ "`"
    show EmptyError = "Empty error"

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input ->
        do (output, x) <- p input
           return (output, f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Right (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input ->
        do (intermediateOutput, f) <- p1 input
           (finalOutput, x) <- p2 intermediateOutput
           return (finalOutput, f x)

instance Alternative Parser where
    empty = Parser $ \_ -> Left [EmptyError]
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        case p1 input of
            Left err -> case p2 input of
                Left err' -> Left $ nub $ err ++ err'
                Right output -> Right output
            Right output -> Right output

instance Monad Parser where
    (Parser p) >>= f = Parser $ \input ->
        do (output, x) <- p input
           runParser (f x) output

token :: (String -> ParserError) -> (Char -> Bool) -> Parser Char
token errType predicate = Parser $ \input ->
    case get input of
        (i, Nothing) -> Left [EndOfInput (inputLoc i)]
        (i, Just c) | predicate c -> Right (i, c)
                    | otherwise   -> Left [errType [c]]

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy expected = token (UnexpectedInput expected)

char :: Char -> Parser Char
char c = satisfy "character `c`" (==c)

string :: String -> Parser String
string = traverse char

spaceP :: Parser String
spaceP = some $ satisfy "space character" isSpace

keywords :: [String]
keywords = ["let", "rec", "in", "if", "then", "else"]

varNameP :: Parser VarName
varNameP = do
    res <- some $ satisfy "a letter" isAlpha
    if res `elem` keywords
        then Parser $ \_ -> Left [ReservedKeyword res]
        else return res

varP :: Parser Expr
varP = Var <$> varNameP

numP :: Parser Integer
numP = read <$> some (satisfy "a digit" isDigit)

literalP :: Parser Expr
literalP = do
    sign <- isJust <$> optional (char '-')
    num  <- numP
    return $ Lit $ (if sign then -1 else 1) * num

parenExprP :: Parser Expr
parenExprP = char '(' *> optional spaceP *> exprP <* optional spaceP <* char ')'

atomP :: Parser Expr
atomP = varP <|> literalP <|> parenExprP

lamP :: Parser Expr
lamP = do
    _       <- char '\\' <* optional spaceP
    varName <- varNameP <* optional spaceP
    _       <- char '.'
    Lam varName <$> exprP

appP :: Parser Expr
appP = do
    atoms <- some (atomP <* optional spaceP)
    return $ foldl1 App atoms

letP :: Parser Expr
letP = do
    _         <- string "let" <* spaceP
    recursive <- isJust <$> optional (string "rec" <* spaceP)
    varName   <- varNameP <* optional spaceP
    _         <- char '=' <* spaceP
    body      <- exprP <* optional spaceP
    _         <- string "in" <* spaceP
    Let recursive varName body <$> exprP

ifP :: Parser Expr
ifP = do
    _      <- string "if" <* spaceP
    predic <- exprP <* optional spaceP
    _      <- string "then" <* spaceP
    ifBr   <- exprP <* optional spaceP
    _      <- string "else" <* spaceP
    Cond predic ifBr <$> exprP

exprP :: Parser Expr
exprP = optional spaceP *> (ifP <|> letP <|> lamP <|> appP) <* optional spaceP

formatErrors :: [ParserError] -> String
formatErrors errs = unlines $ map (("* " ++) . show) errs

parseExpr :: String -> Either String Expr
parseExpr str = case runParser exprP (Input 1 str) of
    Left errs -> Left $ formatErrors errs
    Right (Input i s, x) -> case s of
        "" -> Right x
        _  -> Left $ formatErrors [UnparsedInput i s]
