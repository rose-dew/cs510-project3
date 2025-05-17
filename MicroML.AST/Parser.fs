module MicroML.Parser

open FParsec
open AST

// Parser implementation using FParsec
let pInt = pint32 |>> Int
let pFloat = pfloat |>> Float
let pBool = (stringReturn "true" (Bool true)) <|> (stringReturn "false" (Bool false))
let pString = between (pstring "\"") (pstring "\"") (manyChars (noneOf "\"")) |>> String
let pVar = many1Chars (letter <|> digit) |>> Var

let pExpr, pExprRef = createParserForwardedToRef<Expr, unit>()

let pLet = 
    pipe3 
        (pstring "let" >>. spaces1 >>. many1Chars (letter <|> digit) .>> spaces .>> pstring "=" .>> spaces)
        pExpr 
        (spaces >>. pstring "in" >>. spaces >>. pExpr)
        (fun name e1 e2 -> Let(name, e1, e2))

let pLambda =
    pipe2
        (pstring "fun" >>. spaces1 >>. many1Chars (letter <|> digit) .>> spaces .>> pstring "->" .>> spaces)
        pExpr
        (fun param body -> Lambda(param, body))

let pIf =
    pipe3
        (pstring "if" >>. spaces1 >>. pExpr .>> spaces .>> pstring "then" .>> spaces)
        pExpr
        (spaces >>. pstring "else" >>. spaces >>. pExpr)
        (fun cond thenExpr elseExpr -> If(cond, thenExpr, elseExpr))

let pApply = 
    pipe2 
        (pExpr .>> spaces)
        pExpr
        (fun f arg -> Apply(f, arg))

let pBinOp =
    let opp = new OperatorPrecedenceParser<Expr, unit, unit>()
    let expr = opp.ExpressionParser
    opp.TermParser <- pExpr
    
    opp.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, fun x y -> BinOp("+", x, y)))
    opp.AddOperator(InfixOperator("-", spaces, 1, Associativity.Left, fun x y -> BinOp("-", x, y)))
    opp.AddOperator(InfixOperator("*", spaces, 2, Associativity.Left, fun x y -> BinOp("*", x, y)))
    opp.AddOperator(InfixOperator("/", spaces, 2, Associativity.Left, fun x y -> BinOp("/", x, y)))
    opp.AddOperator(InfixOperator("=", spaces, 0, Associativity.Left, fun x y -> BinOp("=", x, y)))
    
    expr

do pExprRef := choice [
    pLet
    pLambda
    pIf
    pApply
    pBinOp
    pInt
    pFloat
    pBool
    pString
    pVar
    between (pstring "(") (pstring ")") pExpr
]

let parse input =
    match run pExpr input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg