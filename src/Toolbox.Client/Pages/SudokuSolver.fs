module Toolbox.Client.Pages.SudokuSolver

open Feliz
open Elmish
open Feliz.UseElmish
open Feliz.DaisyUI
open Toolbox.Shared.API.SudokuSolver
open Toolbox.Client.Server
open Fable.Core.JsInterop

let emptyBoard: Board =
    [|
        [| None; None; None; None; None; None; None; None; None |]
        [| None; None; None; None; None; None; None; None; None |]
        [| None; None; None; None; None; None; None; None; None |]
        [| None; None; None; None; None; None; None; None; None |]
        [| None; None; None; None; None; None; None; None; None |]
        [| None; None; None; None; None; None; None; None; None |]
        [| None; None; None; None; None; None; None; None; None |]
        [| None; None; None; None; None; None; None; None; None |]
        [| None; None; None; None; None; None; None; None; None |]
    |]

type State = { CurrentBoard: Board; UserBoard: Board; Possibilities: Possibilities; FocusedCell: int*int }

type Msg =
    | CellValueChanged of int * int * int option
    | SolveSudoku of Board
    | SolveSudokuResult of ServerResult<Board>
    | StartNewPuzzle
    | MoveFocus of int * int

let init () = { CurrentBoard = emptyBoard; UserBoard = emptyBoard; Possibilities = getPossibilities emptyBoard; FocusedCell = (0,0) }, Cmd.none

let update (msg:Msg) (state:State) =
    match msg with
    | CellValueChanged (row, col, valueOption) ->
        printfn "Set value for cell (%d, %d) to %A" row col valueOption
        let updatedBoard = Array.copy state.CurrentBoard
        let updatedUserBoard = Array.copy state.UserBoard
        updatedBoard.[row].[col] <- valueOption
        updatedUserBoard.[row].[col] <- valueOption
        { state with CurrentBoard = updatedBoard; UserBoard = updatedUserBoard }, Cmd.none
    | SolveSudoku board ->
        printfn "Solving sudoku puzzle..."
        let updatedBoard, possibilities = updateBoard board (getPossibilities board) 0 0 (board.[0].[0])
        { state with CurrentBoard = updatedBoard; Possibilities = possibilities }, Cmd.OfAsync.eitherAsResult (fun _ -> Toolbox.Client.Server.service.SolveSudoku updatedBoard) SolveSudokuResult
    | SolveSudokuResult (Ok board) -> 
        printfn $"Got sudoku result response: {board}"
        { state with CurrentBoard = board }, Cmd.none
    | SolveSudokuResult (Error error) -> 
        printfn $"Got sudoku result server error: {error}"
        state, Cmd.none
    | StartNewPuzzle ->
        printfn "Starting new puzzle..."
        { state with 
            CurrentBoard = emptyBoard
            UserBoard = emptyBoard 
            Possibilities = getPossibilities emptyBoard 
        }, Cmd.none
    | MoveFocus (dRow, dCol) ->
        let newRow = (fst state.FocusedCell + dRow + 9) % 9
        let newCol = (snd state.FocusedCell + dCol + 9) % 9
        { state with FocusedCell = (newRow, newCol) }, Cmd.none

let validateSingleDigit = function
    | "" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> true
    | _ -> false

let cell (row: int) (col: int) (state: State) dispatch =
    let cellRef = React.useRef None
    let isFilledBySolver = state.CurrentBoard.[row].[col].IsSome && state.UserBoard.[row].[col].IsNone
    let isFocused = (row, col) = state.FocusedCell
    
    React.useEffect (fun () -> if isFocused then cellRef.current?focus() |> ignore)

    Html.td [
        prop.style [
            style.width 50
            style.height 50
            style.borderStyle.solid
            style.borderColor "black"
            style.textAlign.center
            style.backgroundColor (if isFilledBySolver then "green" else "white")
        ]
        prop.children [
            Html.input [
                prop.ref cellRef
                prop.id (sprintf "cell-%d-%d" row col)
                prop.className (sprintf "cell -%d-%d" row col)
                prop.style [ style.width (length.percent 100); style.height (length.percent 100); style.textAlign.center ]
                prop.type'.text
                prop.value (match state.CurrentBoard.[row].[col] with | Some v -> string v | None -> "" )
                prop.maxLength 1
                prop.onChange (fun (newValue: string) -> 
                    if validateSingleDigit newValue then
                        let intValue = if newValue = "" then None else Some <| int newValue
                        CellValueChanged (row, col, intValue) |> dispatch)
                prop.onKeyDown (fun keyEvent -> 
                    match keyEvent.key with
                    | "ArrowUp" -> MoveFocus (-1, 0) |> dispatch
                    | "ArrowDown" -> MoveFocus (1, 0) |> dispatch
                    | "ArrowLeft" -> MoveFocus (0, -1) |> dispatch
                    | "ArrowRight" -> MoveFocus (0, 1) |> dispatch
                    | _ -> ())
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
                Html.p [
                    prop.style [ style.display.flex; style.justifyContent.center; style.alignItems.center ]
                    prop.text "Enter a sudoku puzzle below and click the Solve button to fill it out"
                ]
                Html.br []
                Html.div [
                    prop.style [ style.display.flex; style.justifyContent.center; style.alignItems.center ]
                    prop.children [ 
                        sudokuBoard state dispatch
                    ]
                ]
                Html.br []
                Html.div [
                    prop.style [ style.display.flex; style.justifyContent.center; style.alignItems.center ]
                    prop.children [ 
                        Daisy.button.button [
                            button.accent
                            prop.text "Solve"
                            prop.onClick (fun _ -> SolveSudoku state.CurrentBoard |> dispatch)
                        ]
                        Daisy.button.button [
                            button.ghost
                            prop.text "Clear"
                            prop.onClick (fun _ -> dispatch StartNewPuzzle)
                        ]
                    ]
                ]
            ]
        ]
    ]
