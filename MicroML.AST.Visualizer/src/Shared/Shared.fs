module Shared

type ASTModel = {
    Code: string
    AST: string
}

module Route =
    let builder typeName methodName = sprintf "/api/%s/%s" typeName methodName

type IMicroMLApi = {
    parseCode: string -> Async<Result<AST.Expr, string>>
}