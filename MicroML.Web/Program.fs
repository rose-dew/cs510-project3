module MicroML.Web.Program

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open MicroML.AST

type ASTNode = {
    Type: string
    Value: string option
    Children: ASTNode list
}

let rec exprToNode (expr: Expr) =
    match expr with
    | Int i -> { Type = "Int"; Value = Some (string i); Children = [] }
    | Float f -> { Type = "Float"; Value = Some (string f); Children = [] }
    | Bool b -> { Type = "Bool"; Value = Some (string b); Children = [] }
    | String s -> { Type = "String"; Value = Some s; Children = [] }
    | Var v -> { Type = "Var"; Value = Some v; Children = [] }
    | Let(name, e1, e2) -> 
        { Type = "Let"; Value = Some name; Children = [exprToNode e1; exprToNode e2] }
    | If(cond, thenExpr, elseExpr) ->
        { Type = "If"; Value = None; Children = [exprToNode cond; exprToNode thenExpr; exprToNode elseExpr] }
    | Lambda(param, body) ->
        { Type = "Lambda"; Value = Some param; Children = [exprToNode body] }
    | Apply(f, arg) ->
        { Type = "Apply"; Value = None; Children = [exprToNode f; exprToNode arg] }
    | BinOp(op, left, right) ->
        { Type = "BinOp"; Value = Some op; Children = [exprToNode left; exprToNode right] }
    | UnOp(op, e) ->
        { Type = "UnOp"; Value = Some op; Children = [exprToNode e] }
    | Seq exprs ->
        { Type = "Seq"; Value = None; Children = List.map exprToNode exprs }
    | Tuple exprs ->
        { Type = "Tuple"; Value = None; Children = List.map exprToNode exprs }
    | Unit ->
        { Type = "Unit"; Value = None; Children = [] }

let redirectHandler (context: HttpContext) =
    context.Response.Redirect("/index.html")
    Task.CompletedTask

let parseHandler (context: HttpContext) =
    task {
        try
            let! code = context.Request.ReadFromJsonAsync<string>(cancellationToken = context.RequestAborted)
            match code with
            | null -> 
                context.Response.StatusCode <- StatusCodes.Status400BadRequest
                return! context.Response.WriteAsJsonAsync({| error = "Empty request body" |}, context.RequestAborted)
            | code ->
                try
                    let ast = MicroML.Parser.parse code
                    let node = exprToNode ast
                    return! context.Response.WriteAsJsonAsync(node, context.RequestAborted)
                with ex ->
                    context.Response.StatusCode <- StatusCodes.Status400BadRequest
                    return! context.Response.WriteAsJsonAsync({| error = ex.Message |}, context.RequestAborted)
        with ex ->
            context.Response.StatusCode <- StatusCodes.Status500InternalServerError
            return! context.Response.WriteAsJsonAsync({| error = "Internal server error" |}, context.RequestAborted)
    } :> Task

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    
    // Add services to the container
    builder.Services.AddControllers() |> ignore
    
    let app = builder.Build()
    
    // Configure the HTTP request pipeline
    app.UseStaticFiles()
       .UseRouting()
    
    // Configure endpoints
    app.MapGet("/", redirectHandler)
    app.MapPost("/parse", parseHandler)
    
    app.Run()
    
    0 // Exit code