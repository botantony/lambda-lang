module Main where

import Data.Char (isAlpha)
import Data.List (intercalate)
import Interpreter
import Lang
import Parser
import System.Environment

import System.Console.Haskeline

type ExternalEnv = [(VarName, Expr)]

version :: String
version = "0.1.1"

showExtEnv :: ExternalEnv -> String
showExtEnv extEnv = "[" ++ intercalate ", " (map go extEnv) ++ "]"
    where
        go (varName, expr) = "(" ++ varName ++ ", " ++ formatExpr expr ++ ")"

beginsWith :: String -> String -> Bool
beginsWith _ "" = True
beginsWith "" _ = False
beginsWith (x:xs) (y:ys) = x == y && xs `beginsWith` ys

isValidVarName :: String -> Bool
isValidVarName str = all isAlpha str && str `notElem` keywords
    where
        keywords = ["let", "rec", "in", "if", "then", "else"]

execProgram :: String -> ExternalEnv -> String
execProgram str extEnv = case parseExpr str of
    Left errs  -> errs
    Right expr -> showEval (exprWithEnv extEnv expr)

exprWithEnv :: ExternalEnv -> Expr -> Expr
exprWithEnv [] expr = expr
exprWithEnv ((varName, body):xs) expr
    = Let {
        recursive = True,
        varName = varName,
        body = body,
        expr = exprWithEnv xs expr
    }

repl :: ExternalEnv -> InputT IO ()
repl extEnv = do
    Just s <- getInputLine "> "
    case s of
        "env" -> do
            outputStr "Env: "
            outputStrLn $ showExtEnv extEnv
            repl extEnv
        str | str == "quit" || str == "q" -> return ()
            | str == "help" || str == "h" -> do
                outputStr helpMessage
                repl extEnv
            | str `beginsWith` "set " -> do
                let varName = drop 4 str in evalAndModifyEnv varName True
            | str `beginsWith` "unset " -> do
                let varName = drop 6 str in evalAndModifyEnv varName False
            | otherwise -> do
                outputStrLn $ execProgram str (reverse extEnv)
                repl extEnv
    where
        helpMessage = "Options: `h` or `help`: print this message\n`env`: print current environment\n"
            ++ "`q` or `quit`: quit repl\n`set <name>`: set <name> to be an expression in current environment\n"
            ++ "`unset <name>: remove <name> from the current environment"

        evalAndModifyEnv :: VarName -> Bool -> InputT IO ()
        evalAndModifyEnv varName inserts = case (isValidVarName varName, inserts) of
            (False, _) -> do
                outputStrLn $ "`" ++ varName ++ "` is not a valid variable name"
                repl extEnv
            (_, False) -> repl $ removeFromEnv varName extEnv
            (_, True)  -> addNewVar varName

        removeFromEnv :: String -> ExternalEnv -> ExternalEnv
        removeFromEnv str = filter (\(varName, _) -> varName == str)

        addNewVar :: VarName -> InputT IO ()
        addNewVar varName = do
            Just program <- getInputLine "> "
            case parseExpr program of
                Left errs  -> do
                    outputStrLn errs
                    repl extEnv
                Right expr -> repl $ (varName, expr):extEnv

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> putStrLn usageMessage
        ["-h"] -> putStrLn usageMessage
        ["--version"] -> putStrLn version
        ["-v"] -> putStrLn version
        [file] -> do
            content <- readFile file
            print $ execProgram content []
        [] -> runInputT defaultSettings $ repl []
        _  -> putStrLn $ "Unexpected input!\n" ++ usageMessage
    where
        usageMessage = "Usage: lamda [file]\n"
            ++ "  no input: run repl"
            ++ "  --help or -h: show this page\n"
            ++ "  --version or -v: show version of the program\n"
            ++ "  <file>: execute program from file\n"
