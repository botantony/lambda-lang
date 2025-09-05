module Lang where

type Literal = Integer

type VarName = String

data Expr
    = Var VarName
    | Lit Literal
    | Lam { varName :: VarName, body :: Expr }
    | App { body :: Expr, arg :: Expr }
    | Let { recursive :: Bool, varName :: VarName, body :: Expr, expr :: Expr }
    | Cond { predicate :: Expr, ifBranch :: Expr, elseBranch :: Expr }
    deriving (Show, Eq)

formatExpr :: Expr -> String
formatExpr (Var i) = i
formatExpr (Lit i) = show i
formatExpr (Lam varName body) = "\\" ++ varName ++ "." ++ formatExpr body
formatExpr (App body arg) = formatExpr body ++ case arg of
    Var i -> " " ++ i
    Lit i -> " " ++ show i
    _     -> " (" ++ formatExpr arg ++ ")"
formatExpr (Let recursive varName body expr)
    = "let " ++ (if recursive then "rec " else "") ++ varName ++ " = " ++ formatExpr body ++ " in " ++ formatExpr expr
formatExpr (Cond p ifBr elseBr) = "if " ++ formatExpr p ++ " then " ++ formatExpr ifBr ++ " else " ++ formatExpr elseBr
