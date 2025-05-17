module Server

open Shared
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Microsoft.AspNetCore.Http
open Giraffe
open Parser

let microMLApi : IMicroMLApi = {
    parseCode = fun code -> async {
        match parse code with
        | Ok ast -> return Ok ast
        | fai err -> return Error err
    }
}

let apiRouter : HttpHandler =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue microMLApi
    |> Remoting.buildHttpHandler