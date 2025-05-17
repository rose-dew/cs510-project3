module Client

open Elmish
open Feliz
open Feliz.UseElmish
open Fable.Remoting.Client
open Shared

// If you're using routing (optional)
open Feliz.Router 

type Model = {
    InputCode: string
    AST: Result<AST.Expr, string>
    IsLoading: bool
}

type Msg =
    | UpdateInput of string
    | ParseCode
    | CodeParsed of Result<AST.Expr, string>

let init () = 
    { InputCode = "let x = 5 in x + 3"; AST = Ok (AST.Int 0); IsLoading = false }, Cmd.none

let api =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IMicroMLApi>

let update msg model =
    match msg with
    | UpdateInput code -> { model with InputCode = code }, Cmd.none
    | ParseCode -> 
        { model with IsLoading = true }, 
        Cmd.OfAsync.either api.parseCode model.InputCode CodeParsed (fun ex -> CodeParsed (Error ex.Message))
    | CodeParsed result -> { model with AST = result; IsLoading = false }, Cmd.none

let rec renderAST (ast: AST.Expr) =
    match ast with
    | AST.Var name -> Html.div [ prop.className "ast-node var"; prop.text name ]
    | AST.Int n -> Html.div [ prop.className "ast-node int"; prop.text (string n) ]
    | AST.Float f -> Html.div [ prop.className "ast-node float"; prop.text (string f) ]
    | AST.Bool b -> Html.div [ prop.className "ast-node bool"; prop.text (string b) ]
    | AST.String s -> Html.div [ prop.className "ast-node string"; prop.text s ]
    | AST.Lambda(param, body) ->
        Html.div [
            prop.className "ast-node lambda"
            prop.children [
                Html.div [ prop.className "lambda-param"; prop.text ("λ" + param) ]
                renderAST body
            ]
        ]
    | AST.App(func, arg) ->
        Html.div [
            prop.className "ast-node app"
            prop.children [
                Html.div [ prop.className "app-func"; prop.children [renderAST func] ]
                Html.div [ prop.className "app-arg"; prop.children [renderAST arg] ]
            ]
        ]
    | AST.Let(name, value, body) ->
        Html.div [
            prop.className "ast-node let"
            prop.children [
                Html.div [ prop.className "let-name"; prop.text ("let " + name) ]
                Html.div [ prop.className "let-value"; prop.children [renderAST value] ]
                Html.div [ prop.className "let-body"; prop.children [renderAST body] ]
            ]
        ]
    | AST.If(cond, thenExpr, elseExpr) ->
        Html.div [
            prop.className "ast-node if"
            prop.children [
                Html.div [ prop.className "if-cond"; prop.text "if"; prop.children [renderAST cond] ]
                Html.div [ prop.className "if-then"; prop.text "then"; prop.children [renderAST thenExpr] ]
                Html.div [ prop.className "if-else"; prop.text "else"; prop.children [renderAST elseExpr] ]
            ]
        ]
    | AST.BinOp(op, left, right) ->
        Html.div [
            prop.className "ast-node binop"
            prop.children [
                Html.div [ prop.className "binop-op"; prop.text op ]
                Html.div [ prop.className "binop-left"; prop.children [renderAST left] ]
                Html.div [ prop.className "binop-right"; prop.children [renderAST right] ]
            ]
        ]
    | AST.UnaryOp(op, expr) ->
        Html.div [
            prop.className "ast-node unaryop"
            prop.children [
                Html.div [ prop.className "unaryop-op"; prop.text op ]
                renderAST expr
            ]
        ]
    | AST.Tuple exprs ->
        Html.div [
            prop.className "ast-node tuple"
            prop.children [
                Html.div [ prop.className "tuple-open"; prop.text "(" ]
                for e in exprs do renderAST e
                Html.div [ prop.className "tuple-close"; prop.text ")" ]
            ]
        ]
    | AST.LetRec(name, value, body) ->
        Html.div [
            prop.className "ast-node letrec"
            prop.children [
                Html.div [ prop.className "letrec-name"; prop.text ("let rec " + name) ]
                Html.div [ prop.className "letrec-value"; prop.children [renderAST value] ]
                Html.div [ prop.className "letrec-body"; prop.children [renderAST body] ]
            ]
        ]

let view model dispatch =
    Html.div [
        prop.className "container"
        prop.children [
            Html.h1 [ prop.className "title"; prop.text "MicroML AST Visualizer" ]
            
            Html.div [
                prop.className "input-section"
                prop.children [
                    Html.textarea [
                        prop.className "code-input"
                        prop.value model.InputCode
                        prop.onChange (UpdateInput >> dispatch)
                        prop.placeholder "Enter MicroML code here..."
                    ]
                    Html.button [
                        prop.className "button is-primary"
                        prop.disabled model.IsLoading
                        prop.onClick (fun _ -> dispatch ParseCode)
                        prop.text (if model.IsLoading then "Parsing..." else "Parse Code")
                    ]
                ]
            ]
            
            Html.div [
                prop.className "output-section"
                prop.children [
                    match model.AST with
                    | Ok ast -> renderAST ast
                    | Error err -> 
                        Html.div [
                            prop.className "error-message"
                            prop.text err
                        ]
                ]
            ]
        ]
    ]

let app = React.functionComponent(fun () ->
    let model, dispatch = React.useElmish(init, update, [| |])
    view model dispatch)