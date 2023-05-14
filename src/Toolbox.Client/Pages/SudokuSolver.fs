module Toolbox.Client.Pages.SudokuSolver

open Feliz
open Elmish
open Feliz.UseElmish

type Cell = int option
type Board = Cell list list

type State = { Board: Board }

type Msg =
    | CellValueChanged of int * int * string

let board: Board = [
    [Some 5; None; Some 3; None; None; None; None; None; None];
    [Some 6; None; None; Some 1; Some 9; Some 5; None; None; None];
    [None; Some 9; Some 8; None; None; None; None; Some 6; None];
    [Some 8; None; None; None; Some 6; None; None; None; Some 3];
    [Some 4; None; None; Some 8; None; Some 3; None; None; Some 1];
    [Some 7; None; None; None; Some 2; None; None; None; Some 6];
    [None; Some 6; None; None; None; None; Some 2; Some 8; None];
    [None; None; None; Some 4; Some 1; Some 9; None; None; Some 5];
    [None; None; None; None; Some 8; None; Some 1; None; Some 9];
]

let init () = { Board = board }, Cmd.none

let updateBoard (board:Board) (row:int) (col:int) value : Board =
    
    let updatedRow =
        board
        |> List.item row
        |> List.mapi (fun i cell -> if i = col then value else cell)
    
    board 
    |> List.mapi (fun i currentRow -> if i = row then updatedRow else currentRow)

let update (msg:Msg) (state:State) =
    match msg with
    | CellValueChanged (row, col, value) ->
        printfn "Set value for cell (%d, %d) to %A" row col value
        let updatedBoard = updateBoard state.Board row col (if value = "" then None else Some <| int value)
        { state with Board = updatedBoard }, Cmd.none


let getPossibleValues (board: Board) row col =
    let values = Set [1..9] |> Set.map Some
    let indices = [0..8]
    let rowValues = indices |> List.map (fun i -> List.item i (List.item row board)) |> Set.ofList
    let colValues = indices |> List.map (fun i -> List.item col (List.item i board)) |> Set.ofList
    let boxRow, boxCol = row / 3 * 3, col / 3 * 3
    let boxValues = [for r in boxRow..boxRow + 2 do for c in boxCol..boxCol + 2 -> List.item c (List.item r board)] |> Set.ofList
    Set.difference values (Set.unionMany [rowValues; colValues; boxValues]) |> Set.toList

let applyNakedSingle (board: Board) =
    let mutable updated = false
    let mutable board = board
    for r in 0..8 do
        for c in 0..8 do
            if List.item c (List.item r board) = None then
                let possibleValues = getPossibleValues board r c
                if List.length possibleValues = 1 then
                    board <- updateBoard board r c (List.head possibleValues)
                    updated <- true
    updated

let applyHiddenSingle (board: Board) =
    let mutable updated = false
    let mutable board = board
    for value in [1..9] do
        for r in 0..8 do
            for c in 0..8 do
                if List.item c (List.item r board) = None then
                    let rowPeers = [for i in 0..8 -> List.item i (List.item r board)] |> List.filter ((<>) None)
                    let colPeers = [for i in 0..8 -> List.item c (List.item i board)] |> List.filter ((<>) None)
                    let boxRow, boxCol = r / 3 * 3, c / 3 * 3
                    let boxPeers = [for i in boxRow..boxRow + 2 do for j in boxCol..boxCol + 2 -> List.item j (List.item i board)] |> List.filter ((<>) None)
                    let hiddenSingle = not (List.exists ((=) (Some value)) rowPeers) && not (List.exists ((=) (Some value)) colPeers) && not (List.exists ((=) (Some value)) boxPeers)
                    if hiddenSingle then
                        board <- updateBoard board r c (Some value)
                        updated <- true
    updated

let rec solveSudoku (board: Board) =
    let applyStrategies board =
        let rec applyStrategiesTailRec board updated =
            if updated then
                let nakedSingleApplied = applyNakedSingle board
                let hiddenSingleApplied = applyHiddenSingle board
                applyStrategiesTailRec board (nakedSingleApplied || hiddenSingleApplied)
            else
                board
        applyStrategiesTailRec board true

    let updatedBoard = applyStrategies board
    let emptyCell = Seq.tryFind (fun (r, c) -> (List.item c (List.item r updatedBoard)) = None) [for r in 0..8 do for c in 0..8 -> (r, c)]

    emptyCell
    |> Option.map (fun (row, col) ->
        getPossibleValues updatedBoard row col
        |> List.tryPick (fun value ->
            let newBoard = List.map (List.map id) updatedBoard
            let updatedBoard = updateBoard newBoard row col value
            solveSudoku updatedBoard
        )
    )
    |> Option.defaultWith (fun () -> Some updatedBoard)


let cell (row: int) (col: int)  =
    let state, dispatch = React.useElmish(init, update)
    Html.td [
        prop.style [
            style.width 50
            style.height 50
            style.borderStyle.solid
            style.borderWidth 1
            style.borderColor "black"
            style.textAlign.center
        ]
        prop.children [
            Html.input [
                prop.id (sprintf "cell-%d-%d" row col)
                prop.className (sprintf "cell-%d-%d" row col)
                prop.style [ style.width (length.percent 100); style.height (length.percent 100); style.textAlign.center ]
                prop.type'.text
                prop.value (if state.Board.[row].[col] = None then "" else state.Board.[row].[col] |> string)
                prop.maxLength 1
                prop.onChange (fun (newValue: string) -> CellValueChanged (row, col, newValue) |> dispatch)
            ]
        ]
    ]


let row (rowIndex: int) (rowData: Cell list) =
    Html.tr [
        prop.children [ for (colIndex, cellValue) in rowData |> List.indexed -> cell rowIndex colIndex ]
    ]

let sudokuBoard (board: Board) =
    Html.table [
        Html.tbody [
            prop.children [ for (rowIndex, boardRow) in board |> List.indexed -> row rowIndex boardRow ]
        ]
    ]

[<ReactComponent>]
let SudokuSolverView () =
    React.fragment[
        Html.div [
            prop.children [
                Html.h1 "Sudoku Solver"
                Html.p "Enter the sudoku puzzle below and click the Solve button to solve it."
                Html.br []
                Html.div [
                    prop.style [ style.display.flex; style.justifyContent.center; style.alignItems.center ]
                    prop.children [ sudokuBoard board ]
                ]
            ]
        ]
    ]