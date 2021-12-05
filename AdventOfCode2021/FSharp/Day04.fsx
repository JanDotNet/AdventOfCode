open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day04.txt")

// let testInput = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

// 22 13 17 11  0
//  8  2 23  4 24
// 21  9 14 16  7
//  6 10  3 18  5
//  1 12 20 15 19

//  3 15  0  2 22
//  9 18 13 17  5
// 19  8  7 25 23
// 20 11 10 24  4
// 14 21 16 12  6

// 14 21 17 24  4
// 10 16 15  9 19
// 18  8 23 26 20
// 22 11 13  6  5
//  2  0 12  3  7"

// let input = testInput.Split('\n') |> Array.toList

let input = File.ReadAllLines(file) |> Array.toList

type Position = { Row:int; Col:int; Value:int; Checked:bool }
type Board = {Positions: Position list} with
    
    static member Empty = {Positions = []}
    
    static member getSize board =
        match board.Positions with
        | [] -> 0,0
        | pos -> let maxRow = pos |> List.map (fun p -> p.Row ) |> List.max
                 let maxCol = pos |> List.map (fun p -> p.Col ) |> List.max
                 maxRow, maxCol
    
    static member applyDrawing drawing board =
        let applyCheck pos = {pos with Checked = pos.Checked || (drawing = pos.Value)}
        { Positions = board.Positions |> List.map applyCheck }

    static member sumUnchecked board =
        board.Positions |> List.filter (fun p -> not p.Checked) |> List.sumBy (fun p -> p.Value)
    
    static member isWinningRow row board = board.Positions |> List.filter (fun p -> p.Row = row ) |> List.forall (fun p -> p.Checked)
    static member isWinningCol col board = board.Positions |> List.filter (fun p -> p.Col = col ) |> List.forall (fun p -> p.Checked)

    static member isWinning board =
        let (rows, cols) = board |> Board.getSize
        let rec check rowOrCol maxRowOrCol checker =
            if board |> (checker rowOrCol) then true
            elif rowOrCol = maxRowOrCol then false
            else check (rowOrCol + 1) maxRowOrCol checker
        check 1 rows Board.isWinningRow || check 1 cols Board.isWinningCol

let parseNumbers (puzzleInput:string list) =
    let drawNumbersStr = puzzleInput |> List.head
    drawNumbersStr.Split(',') |> Array.toList |> List.map int

let boardFolder ((boardList:Board list), currentBorad:Board) line =
    match line with
    | "" -> (currentBorad :: boardList, Board.Empty)
    | _  -> let maxRow = ((currentBorad |> Board.getSize) |> fst)
            let positionsRow = line.Split(' ', StringSplitOptions.RemoveEmptyEntries) 
                                |> Seq.indexed 
                                |> Seq.map (fun (idx, value) -> { Row = maxRow+1; Col = idx+1; Value = int(value); Checked = false})
                                |> Seq.toList
            (boardList, {Positions = currentBorad.Positions @ positionsRow })

let drawings = input |> parseNumbers
let firstBoards, lastBoard = input |> List.skip 2 |> List.fold boardFolder ([], Board.Empty)
let boards = (lastBoard :: firstBoards) |> List.rev

let rec solve1 (ds:int list) (bs: Board list) =
    let d = ds |> List.head
    let bs' = bs |> List.map (Board.applyDrawing d)
    match bs' |> List.tryFind (fun b -> b |> Board.isWinning) with
    | Some(winningBoard) ->         
        (winningBoard |> Board.sumUnchecked) * d
    | None -> solve1 ds.Tail bs'

let rec solve2 (ds:int list) (bs: Board list) =
    let d = ds |> List.head
    let bs' = bs |> List.map (Board.applyDrawing d)
    let allWinning = bs' |> List.filter (fun b -> b |> Board.isWinning)
    let allNonWinning = bs' |> List.except allWinning
    match bs', allWinning with
    | _, [] -> solve2 ds.Tail bs'
    | [rem], [winning] -> (winning |> Board.sumUnchecked) * d        
    | _ -> solve2 ds.Tail allNonWinning

solve1 drawings boards
solve2 drawings boards