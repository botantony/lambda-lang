module Interpreter (
    Closure(..),
    Value(..),
    Env,
    EvalError(..),
    eval,
    showEval,
    printEval,
    defaultEnv,
    binOp
) where

import Data.Map (Map, insert, (!?), fromList)
-- import Debug.Trace (trace)
import Lang

type ResultValue = Either EvalError Value
type Env = Map VarName Thunk

newtype Thunk = Thunk ResultValue

instance Show Thunk where
    show _ = "<thunk>"

data Value
    = ValueInt Integer
    | ValueClosure Closure
    | ValueBuiltin (Value -> ResultValue)

instance Show Value where
    show (ValueInt i) = show i
    show (ValueClosure _) = "Closure"
    show (ValueBuiltin _) = "Builtin"

data Closure = Closure { closureEnv :: Env, closureVar :: VarName, closureExpr :: Expr }
    deriving Show

data EvalError
    = UnboundName VarName
    | NoneError VarName
    | WrongType Value String
    | NotAFunction Value
    | ArithmeticError String
    | CustomError String

instance Show EvalError where
    show (UnboundName x) = "Unbound variable with name `" ++ x ++ "`"
    show (NoneError x) = "Variable with name `" ++ x ++ "` has no value"
    show (WrongType val valType) = "Variable `" ++ show val ++ "` has wrong type (expected " ++ valType ++ ")"
    show (NotAFunction val) = "Value `" ++ show val ++ "` is not a function"
    show (ArithmeticError cause) = "Arithmetic error: " ++ cause
    show (CustomError cause) = cause

force :: Thunk -> ResultValue
force (Thunk v) = v

thunk :: Env -> Expr -> Thunk
thunk env e = Thunk (eval env e)

valueAsThunk :: Value -> Thunk
valueAsThunk v = Thunk (Right v)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a _        = Left a

valueAsInt :: Value -> Either EvalError Integer
valueAsInt (ValueInt i) = Right i
valueAsInt other        = Left (WrongType other "int")

eval :: Env -> Expr -> ResultValue
eval _ (Lit i) = return (ValueInt i)

eval env (Var x) = maybeToEither (UnboundName x) (env !? x) >>= force

eval env (Lam var expr) = return $ ValueClosure $ Closure { closureEnv = env, closureVar = var, closureExpr = expr }

eval env (App abst arg) = do
    abstVal <- eval env abst
    case abstVal of
        ValueClosure (Closure clEnv varName body) -> do
            let argTh = thunk env arg
            eval (insert varName argTh clEnv) body
        ValueBuiltin fn -> do
            argVal <- eval env arg
            fn argVal
        _ -> Left $ NotAFunction abstVal

eval env (Cond predicate ifTrue ifFalse) = do
    i <- eval env predicate >>= valueAsInt
    if i /= 0 then eval env ifTrue else eval env ifFalse

eval env (Let rec varName body expr)
    | not rec =
        let env' = insert varName (thunk env body) env
        in eval env' expr
    | otherwise =
        let envRec = insert varName th env
            th     = thunk envRec body
        in eval envRec expr

showEval :: Expr -> String
showEval expr = case eval defaultEnv expr of
    Left err -> show err
    Right val -> show val

printEval :: Expr -> IO ()
printEval = print . showEval

fnConstructor :: (Integer -> Integer -> Integer) -> Value
fnConstructor fn = ValueBuiltin $ \x ->
    return $ ValueBuiltin $ \y -> do
        i <- valueAsInt x
        j <- valueAsInt y
        return (ValueInt (fn i j))

fnConstructorNotZero :: (Integer -> Integer -> Integer) -> Value
fnConstructorNotZero fn = ValueBuiltin $ \x ->
    return $ ValueBuiltin $ \y -> do
        i <- valueAsInt x
        j <- valueAsInt y
        if j == 0 then Left (ArithmeticError "second argument should not be 0") else return (ValueInt (fn i j))

compExpr :: (Integer -> Integer -> Bool) -> Value
compExpr fn = ValueBuiltin $ \x ->
    return $ ValueBuiltin $ \y -> do
        i <- valueAsInt x
        j <- valueAsInt y
        return (ValueInt (if fn i j then 1 else 0))

defaultEnv :: Env
defaultEnv = fromList [
    ("add", valueAsThunk $ fnConstructor (+)),
    ("sub", valueAsThunk $ fnConstructor (-)),
    ("mul", valueAsThunk $ fnConstructor (*)),
    ("div", valueAsThunk $ fnConstructorNotZero div),
    ("mod", valueAsThunk $ fnConstructorNotZero mod),
    ("less", valueAsThunk $ compExpr (<)),
    ("eq", valueAsThunk $ compExpr (==)),
    ("greater", valueAsThunk $ compExpr (>))
    ]

binOp :: VarName -> Expr -> Expr -> Expr
binOp fn expr = App (App (Var fn) expr)
