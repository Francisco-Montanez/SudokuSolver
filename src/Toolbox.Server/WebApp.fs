module Toolbox.Server.WebApp

open Giraffe
open Giraffe.GoodRead
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Microsoft.Extensions.Logging
open Toolbox.Shared.API
open Toolbox.Shared.Errors

let service = {
    GetMessage = fun success ->
        task {
            if success then return "Hi from Server!"
            else return ServerError.failwith (ServerError.Exception "OMG, something terrible happened")
        }
        |> Async.AwaitTask

    SolveSudoku = fun initalBoard currentBoard ->
        task {
            let solvedSudoku = Toolbox.Shared.API.SudokuSolver.solveSudoku initalBoard currentBoard (Toolbox.Shared.API.SudokuSolver.getPossibilities currentBoard)
            printfn $"Solved sudoku puzzle: {solvedSudoku}"
            return (if Option.isNone solvedSudoku then currentBoard else solvedSudoku.Value)
        }
        |> Async.AwaitTask
}

let webApp : HttpHandler =
    let remoting logger =
        Remoting.createApi()
        |> Remoting.withRouteBuilder Service.RouteBuilder
        |> Remoting.fromValue service
        |> Remoting.withErrorHandler (Remoting.errorHandler logger)
        |> Remoting.buildHttpHandler
    choose [
        Require.services<ILogger<_>> remoting
        htmlFile "public/index.html"
    ]