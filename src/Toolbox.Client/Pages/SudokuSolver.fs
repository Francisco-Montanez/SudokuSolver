module Toolbox.Client.Pages.SudokuSolver

open Feliz
open Elmish
open Feliz.UseElmish
open Feliz.DaisyUI
open Toolbox.Shared.API.SudokuSolver
open Toolbox.Client.Server

type State = { CurrentBoard: Board; InitialBoard: Board; Possibilities: Possibilities }

type Msg =
    | CellValueChanged of int * int * string
    | SolveSudoku of Board
    | SolveSudokuResult of ServerResult<Board>

let initboard: Board =
    [|
        [| Some 1; Some 9; None; Some 8; Some 5; Some 2; Some 4; Some 6; Some 7 |] // 3
        [| Some 5; Some 2; Some 4; Some 6; Some 7; Some 1; Some 3; Some 9; Some 8 |]
        [| Some 8; Some 7; Some 6; Some 3; Some 9; Some 4; Some 5; Some 2; Some 1 |]
        [| Some 9; Some 5; Some 2; Some 4; Some 1; Some 6; Some 7; Some 8; Some 3 |]
        [| Some 3; Some 1; Some 8; Some 5; Some 2; Some 7; Some 9; Some 4; Some 6 |]
        [| Some 6; Some 4; Some 7; Some 9; Some 3; Some 8; Some 2; Some 1; Some 5 |]
        [| Some 4; Some 3; Some 9; Some 1; Some 6; Some 5; Some 8; Some 7; Some 2 |]
        [| Some 7; Some 6; Some 5; Some 2; Some 8; Some 9; Some 1; Some 3; Some 4 |]
        [| Some 2; Some 8; Some 1; Some 7; Some 4; Some 3; Some 6; Some 5; Some 9 |]
    |]

let init () = { InitialBoard = initboard; CurrentBoard = initboard; Possibilities = getPossibilities initboard }, Cmd.none

let update (msg:Msg) (state:State) =
    match msg with
    | CellValueChanged (row, col, value) ->
        printfn "Set value for cell (%d, %d) to %A" row col value
        let updatedBoard, possibilities = updateBoard state.InitialBoard  state.CurrentBoard (getPossibilities state.CurrentBoard) row col (if value = "" then None else Some <| int value)
        { state with CurrentBoard = updatedBoard; Possibilities = possibilities }, Cmd.none
    | SolveSudoku board ->
        printfn "Solving sudoku puzzle..."
        state, Cmd.OfAsync.eitherAsResult (fun _ -> Toolbox.Client.Server.service.SolveSudoku state.InitialBoard board) SolveSudokuResult
    | SolveSudokuResult (Ok board) -> 
        printfn $"Got sudoku result response: {board}"
        { state with CurrentBoard = board }, Cmd.none
    | SolveSudokuResult (Error error) -> 
        printfn $"Got sudoku result server error: {error}"
        state, Cmd.none

let cell (row: int) (col: int) (state: State) dispatch =
    let isFilledBySolver = state.InitialBoard.[row].[col].IsNone && state.CurrentBoard.[row].[col].IsSome
    Html.td [
        prop.style [
            style.width 50
            style.height 50
            style.borderStyle.solid
            style.borderWidth 1
            style.borderColor "black"
            style.textAlign.center
            style.backgroundColor (if isFilledBySolver then "lightgreen" else "white")
        ]
        prop.children [
            Html.input [
                prop.id (sprintf "cell-%d-%d" row col)
                prop.className (sprintf "cell-%d-%d" row col)
                prop.style [ style.width (length.percent 100); style.height (length.percent 100); style.textAlign.center ]
                prop.type'.text
                prop.value (match state.CurrentBoard.[row].[col] with None -> "" | Some v -> string v)
                prop.maxLength 1
                prop.onChange (fun (newValue: string) -> CellValueChanged (row, col, newValue) |> dispatch)
            ]
        ]
    ]

let row (rowIndex: int) (state: State) dispatch =
    Html.tr [ prop.children [ for colIndex in 0..8 -> cell rowIndex colIndex state dispatch ] ]

let sudokuBoard (state: State) dispatch =
    Html.table [ Html.tbody [ prop.children [ for rowIndex in 0..8 -> row rowIndex state dispatch ] ] ]

[<ReactComponent>]
let SudokuSolverView () =
    let state, dispatch = React.useElmish(init, update)
    React.fragment [
        Html.div [
            prop.children [
                Html.h1 "Sudoku Solver"
                Html.p "Enter the sudoku puzzle below and click the Solve button to solve it."
                Html.br []
                Html.div [
                    prop.style [ style.display.flex; style.justifyContent.center; style.alignItems.center ]
                    prop.children [ 
                        sudokuBoard state dispatch
                        Daisy.button.button [
                            button.outline
                            button.primary
                            prop.text "Solve"
                            prop.onClick (fun _ -> SolveSudoku state.CurrentBoard |> dispatch)
                        ]
                    ]
                ]
            ]
        ]
    ]
