module MicroML.AST

type Expr =
    | Int of int
    | Float of float
    | Bool of bool
    | String of string
    | Var of string
    | Let of string * Expr * Expr
    | If of Expr * Expr * Expr
    | Lambda of string * Expr
    | Apply of Expr * Expr
    | BinOp of string * Expr * Expr
    | UnOp of string * Expr
    | Seq of Expr list
    | Tuple of Expr list
    | Unit