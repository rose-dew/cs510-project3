module AST

type Expr =
    | Var of string
    | Int of int
    | Float of float
    | Bool of bool
    | String of string
    | Lambda of string * Expr
    | App of Expr * Expr
    | Let of string * Expr * Expr
    | If of Expr * Expr * Expr
    | BinOp of string * Expr * Expr
    | UnaryOp of string * Expr
    | Tuple of Expr list
    | LetRec of string * Expr * Expr