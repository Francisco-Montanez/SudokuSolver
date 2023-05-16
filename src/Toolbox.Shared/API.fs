module Toolbox.Shared.API

module SudokuSolver =

    type Cell = int option

    type Board = Cell[][]

    type Possibilities = Set<int>[][]

    let updateBoard (board:Board) (possibilities:Possibilities) (row:int) (col:int) value : Board * Possibilities =
        let updatedRow = Array.copy board.[row]
        updatedRow.[col] <- value
        let updatedBoard = Array.copy board
        updatedBoard.[row] <- updatedRow
        let updatedPossibilities = Array.init 9 (fun i -> Array.init 9 (fun j -> 
            if i = row && j = col then Set.empty 
            else if i = row || j = col || (i / 3 = row / 3 && j / 3 = col / 3) then 
                match value with
                | Some v -> Set.remove v possibilities.[i].[j]
                | None -> possibilities.[i].[j]
            else possibilities.[i].[j]))
        updatedBoard, updatedPossibilities

    let applyNakedSingle (board: Board) (possibilities: Possibilities) =
        let mutable updated = false
        let mutable board = board
        let mutable possibilities = possibilities
        for r in 0..8 do
            for c in 0..8 do
                if board.[r].[c] = None && Set.count possibilities.[r].[c] = 1 then
                    let value = possibilities.[r].[c] |> Seq.head |> Some
                    let newBoard, newPossibilities = updateBoard board possibilities r c value
                    board <- newBoard
                    possibilities <- newPossibilities
                    updated <- true
        updated, board, possibilities

    let applyHiddenSingle (board: Board) (possibilities: Possibilities) =
        let mutable updated = false
        let mutable board = board
        let mutable possibilities = possibilities
        for r in 0..8 do
            for c in 0..8 do
                if board.[r].[c] = None then
                    for value in possibilities.[r].[c] do
                        let singleInRow = Array.forall (fun x -> not (Set.contains value x)) (Array.init 9 (fun i -> if i = c then Set.empty else possibilities.[r].[i]))
                        let singleInCol = Array.forall (fun x -> not (Set.contains value x)) (Array.init 9 (fun i -> if i = r then Set.empty else possibilities.[i].[c]))
                        let singleInBox = Array.forall (fun x -> not (Set.contains value x)) (Array.init 9 (fun i -> if (i / 3 = r / 3 && i % 3 = c / 3) then Set.empty else possibilities.[3 * (r / 3) + i / 3].[3 * (c / 3) + i % 3]))
                        if singleInRow || singleInCol || singleInBox then
                            let newBoard, newPossibilities = updateBoard board possibilities r c (Some value)
                            board <- newBoard
                            possibilities <- newPossibilities
                            updated <- true
        updated, board, possibilities

    let getPossibilities (board: Board) : Possibilities =
        Array.init 9 (fun row ->
            Array.init 9 (fun col ->
                match board.[row].[col] with
                | Some value -> Set.singleton value
                | None -> Set.ofList [1..9]
            )
        )

    let rec solveSudoku (board: Board) (possibilities: Possibilities) =
        let rec applyStrategies board possibilities =
            let updated, newBoard, newPossibilities = applyNakedSingle board possibilities
            if updated then applyStrategies newBoard newPossibilities
            else
                let updated, newBoard, newPossibilities = applyHiddenSingle board possibilities
                if updated then applyStrategies newBoard newPossibilities
                else newBoard, newPossibilities
        let updatedBoard, updatedPossibilities = applyStrategies board possibilities
        let emptyCell = Seq.tryFind (fun (r, c) -> updatedBoard.[r].[c] = None) [for r in 0..8 do for c in 0..8 -> (r, c)]
        match emptyCell with
        | Some (row, col) ->
            updatedPossibilities.[row].[col]
            |> Seq.tryPick (fun value ->
                let newBoard, newPossibilities = updateBoard (Array.map Array.copy updatedBoard) (Array.map Array.copy updatedPossibilities) row col (Some value)
                solveSudoku newBoard newPossibilities
            )
        | None -> Some updatedBoard

open SudokuSolver

type Service = {
    GetMessage : bool -> Async<string>
    SolveSudoku: Board -> Async<Board>
}
with
    static member RouteBuilder _ m = sprintf "/api/service/%s" m
