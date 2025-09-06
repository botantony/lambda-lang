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

import Control.Monad.State
import qualified Data.Bifunctor as Bif
import Data.Map (Map, insert, (!?), fromList, empty)
import Lang

type ResultValue = Either EvalError Value
type Env = Map VarName Thunk

newtype Thunk = Thunk ResultValue deriving (Eq, Ord)

instance Show Thunk where
    show _ = "<thunk>"

data Value
    = ValueInt Integer
    | ValueClosure Closure
    | ValueBuiltin String (Value -> ResultValue)

instance Eq Value where
    (ValueInt i1) == (ValueInt i2) = i1 == i2
    (ValueClosure c1) == (ValueClosure c2) = c1 == c2
    (ValueBuiltin n1 _) == (ValueBuiltin n2 _) = n1 == n2
    _ == _ = False

instance Ord Value where
    (ValueInt i1) <= (ValueInt i2) = i1 <= i2
    (ValueInt _) <= _ = False
    _ <= (ValueInt _) = True
    (ValueClosure c1) <= (ValueClosure c2) = c1 <= c2
    (ValueClosure _) <= (ValueBuiltin _ _) = False
    (ValueBuiltin _ _) <= (ValueClosure _) = True
    (ValueBuiltin n1 _) <= (ValueBuiltin n2 _) = n1 <= n2

instance Show Value where
    show (ValueInt i) = show i
    show (ValueClosure c) = "Closure: " ++ show c
    show (ValueBuiltin n _) = "Builtin (" ++ n ++ ")"

type ClosureId = Int

data Closure = Closure { closureId :: ClosureId, closureEnv :: Env, closureVar :: VarName, closureExpr :: Expr }
    deriving (Show, Eq, Ord)

data EvalError
    = UnboundName VarName
    | NoneError VarName
    | WrongType Value String
    | NotAFunction Value
    | ArithmeticError String
    | CustomError String
    deriving (Eq, Ord)

instance Show EvalError where
    show (UnboundName x) = "Unbound variable with name `" ++ x ++ "`"
    show (NoneError x) = "Variable with name `" ++ x ++ "` has no value"
    show (WrongType val valType) = "Variable `" ++ show val ++ "` has wrong type (expected " ++ valType ++ ")"
    show (NotAFunction val) = "Value `" ++ show val ++ "` is not a function"
    show (ArithmeticError cause) = "Arithmetic error: " ++ cause
    show (CustomError cause) = cause

type CacheKey = (ClosureId, Value)
type Cache = (Map CacheKey ResultValue, ClosureId)

force :: Thunk -> ResultValue
force (Thunk v) = v

thunk :: Env -> Expr -> Thunk
thunk env e = Thunk (runEval env e)

valueAsThunk :: Value -> Thunk
valueAsThunk v = Thunk (Right v)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a _        = Left a

valueAsInt :: Value -> Either EvalError Integer
valueAsInt (ValueInt i) = Right i
valueAsInt other        = Left (WrongType other "int")

runEval :: Env -> Expr -> ResultValue
runEval env expr = evalStateT (evalCache env expr) (empty, 0)

eval :: Env -> Expr -> ResultValue
eval = runEval


evalCache :: Env -> Expr -> StateT Cache (Either EvalError) Value
evalCache _ (Lit i) = return $ ValueInt i
evalCache env (Var x) = lift $ maybeToEither (UnboundName x) (env !? x) >>= force
evalCache env (Lam var expr) = do
    cache <- get
    let (_, oldId) = cache
        clId = oldId + 1
    modify (\(m, _) -> (m, clId))
    return $ ValueClosure $ Closure { closureId = clId, closureEnv = env, closureVar = var, closureExpr = expr }
evalCache env (Cond predicate ifTrue ifFalse) = do
    i <- evalCache env predicate
    case valueAsInt i of
        (Left err) -> lift $ Left err
        Right num -> if num /= 0 then evalCache env ifTrue else evalCache env ifFalse
evalCache env (Let rec varName body expr)
    | not rec = do
        let env' = insert varName (thunk env body) env
        evalCache env' expr
    | otherwise = do
        let envRec = insert varName th env
            th     = thunk envRec body
        evalCache envRec expr
evalCache env (App abst arg) = do
    abstVal <- evalCache env abst
    cache <- get
    let (cacheMap, _) = cache
    case abstVal of
        ValueClosure (Closure clId clEnv varName body) -> do
            argVal <- evalCache env arg
            case cacheMap !? (clId, argVal) of
                Just v -> lift v
                Nothing -> do
                    let env' = insert varName (valueAsThunk argVal) clEnv
                    res <- evalCache env' body
                    modify (Bif.first $ insert (clId, argVal) (Right res))
                    return res
        ValueBuiltin _ fn -> do
            argVal <- evalCache env arg
            lift $ fn argVal
        _ -> lift $ Left $ NotAFunction abstVal

showEval :: Expr -> String
showEval expr = case runEval defaultEnv expr of
    Left err -> show err
    Right val -> show val

printEval :: Expr -> IO ()
printEval = print . showEval

fnConstructor :: String -> (Integer -> Integer -> Integer) -> Value
fnConstructor name fn = ValueBuiltin name $ \x ->
    return $ ValueBuiltin (name ++ ":" ++ show x) $ \y -> do
        i <- valueAsInt x
        j <- valueAsInt y
        return (ValueInt (fn i j))

fnConstructorNotZero :: String -> (Integer -> Integer -> Integer) -> Value
fnConstructorNotZero name fn = ValueBuiltin name $ \x ->
    return $ ValueBuiltin (name ++ ":" ++ show x) $ \y -> do
        i <- valueAsInt x
        j <- valueAsInt y
        if j == 0 then Left (ArithmeticError "second argument should not be 0") else return (ValueInt (fn i j))

compExpr :: String -> (Integer -> Integer -> Bool) -> Value
compExpr name fn = ValueBuiltin name $ \x ->
    return $ ValueBuiltin (name ++ ":" ++ show x) $ \y -> do
        i <- valueAsInt x
        j <- valueAsInt y
        return (ValueInt (if fn i j then 1 else 0))

defaultEnv :: Env
defaultEnv = fromList [
    ("add", valueAsThunk $ fnConstructor "add" (+)),
    ("sub", valueAsThunk $ fnConstructor "sub" (-)),
    ("mul", valueAsThunk $ fnConstructor "mul" (*)),
    ("div", valueAsThunk $ fnConstructorNotZero "div" div),
    ("mod", valueAsThunk $ fnConstructorNotZero "mod" mod),
    ("less", valueAsThunk $ compExpr "less" (<)),
    ("eq", valueAsThunk $ compExpr "eq" (==)),
    ("greater", valueAsThunk $ compExpr "greater" (>))
    ]

binOp :: VarName -> Expr -> Expr -> Expr
binOp fn expr = App (App (Var fn) expr)
