module Parser

open FParsec
open AST
open FParsec.Primitives
open FParsec.CharParsers

let ws = spaces
let str s = pstring s
let str_ws s = str s .>> ws
let betweenStrings s1 s2 p = between (str_ws s1) (str_ws s2) p

// 1. Forward declaration first
let expr, exprRef = createParserForwardedToRef<Expr, unit>()

let identifier : Parser<string, unit> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2 isIdentifierFirstChar isIdentifierChar .>> ws

// Basic expressions
let intExpr = pint32 .>> ws |>> Int
let floatExpr = pfloat .>> ws |>> Float
let boolExpr = (stringReturn "true" (Bool true)) <|> (stringReturn "false" (Bool false)) .>> ws
let stringExpr = between (str "\"") (str "\"") (manySatisfy ((<>) '"')) .>> ws |>> String
let varExpr = identifier |>> Var

// Complex expressions
let tupleExpr = betweenStrings "(" ")" (sepBy expr (str_ws ",")) |>> Tuple

let lambdaExpr =
    pipe2 (str_ws "fun" >>. identifier .>> str_ws "->") expr (fun param body -> Lambda(param, body))

let letExpr =
    pipe3 (str_ws "let" >>. identifier) (str_ws "=" >>. expr) (str_ws "in" >>. expr)
        (fun name value body -> Let(name, value, body))

let letRecExpr =
    pipe3 (str_ws "let" >>. str_ws "rec" >>. identifier) (str_ws "=" >>. expr) (str_ws "in" >>. expr)
        (fun name value body -> LetRec(name, value, body))

let ifExpr =
    pipe3 (str_ws "if" >>. expr) (str_ws "then" >>. expr) (str_ws "else" >>. expr)
        (fun cond thenExpr elseExpr -> If(cond, thenExpr, elseExpr))


// 2. Define all other parsers that use 'expr' recursively
let atomExpr = 
    choice [
        intExpr
        floatExpr
        boolExpr
        stringExpr
        tupleExpr
        lambdaExpr
        letExpr
        letRecExpr
        ifExpr
        varExpr
        betweenStrings "(" ")" expr
    ]

// Helper function to create binary operators
let binop op = InfixOperator(op, ws, 6, Associativity.Left, 
                    fun x y -> BinOp(op, x, y))

// Helper function to create prefix operators
let prefixop op = PrefixOperator(op, ws, 8, true, 
                     fun x -> UnaryOp(op, x))

// 3. Set up operator precedence parser
let opp = new OperatorPrecedenceParser<Expr, unit, unit>()
opp.TermParser <- atomExpr
opp.AddOperator(binop "+")
opp.AddOperator(binop "-")
opp.AddOperator(binop "*")
opp.AddOperator(binop "/")
opp.AddOperator(binop "=")
opp.AddOperator(binop "<") 
opp.AddOperator(binop ">")
opp.AddOperator(prefixop "not")

// 4. Complete the reference - this must come AFTER opp is fully set up
exprRef := opp.ExpressionParser

// 5. Now you can use 'expr' in your parse function
let parse input =
    match run (ws >>. expr .>> eof) input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg