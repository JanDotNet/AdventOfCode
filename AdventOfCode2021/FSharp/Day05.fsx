open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day05.txt")

// let testInput = "0,9 -> 5,9
// 8,0 -> 0,8
// 9,4 -> 3,4
// 2,2 -> 2,1
// 7,0 -> 7,4
// 6,4 -> 2,0
// 0,9 -> 2,9
// 3,4 -> 1,4
// 0,0 -> 8,8
// 5,5 -> 8,2"

//let input = testInput.Split('\n') |> Array.toList

let input = File.ReadAllLines(file) |> Array.toList

type Line = { Start:(int*int); End:(int*int); Points:(int*int) list } with
    static member create p1 p2 =
        let (x1, y1) = p1
        let (x2, y2) = p2
        let incrementor a b = if a = b then (fun p -> p+0)
                              elif a < b then (fun p -> p+1)
                              else (fun p -> p-1)
        let incX = incrementor x1 x2
        let incY = incrementor y1 y2
        let rec generatePoints points point =
            let x, y = point
            let points' = point :: points
            let point' = (incX x, incY y)
            if point = p2 
            then points' 
            else generatePoints points' point'

        { Start = p1; End = p2; Points = generatePoints [] p1 }

    static member isHorizontal line = (line.Start |> snd) = (line.End |> snd)
    static member isVertical line = (line.Start |> fst) = (line.End |> fst)
            

type Board = {Positions: Map<int*int,int>} with
    
    static member Empty = {Positions = Map.empty}
        
    static member applyLine line board =
        let rec applyPoints points (positions:Map<int*int,int>) =
            match points with
            | [] -> positions
            | point'::points' -> let value' = if positions.ContainsKey point' then positions.[point'] + 1 else 1
                                 let positions' = positions |> Map.add point' value'
                                 applyPoints points' positions' 

        { Positions = applyPoints line.Points board.Positions }

    static member applyLines lines board =
        match lines with
        | [] -> board
        | line::lines' -> let board' = board |> Board.applyLine line
                          board' |> Board.applyLines lines'

let parse (lines:string list) =
    let generator = seq {
        for line in lines do
            let splitted = line.Split([|" -> "|], StringSplitOptions.RemoveEmptyEntries)
            let leftSplitted = splitted.[0].Split(',')
            let rightSplitted = splitted.[1].Split(',')
            let p1 = (leftSplitted.[0] |> int, leftSplitted.[1] |> int)
            let p2 = (rightSplitted.[0] |> int, rightSplitted.[1] |> int)
            yield Line.create p1 p2 
            }
    generator |> Seq.toList

let allLines = input |> parse
let horizontalAndVerticalLines = allLines |> List.filter (fun l -> l |> Line.isHorizontal || l |> Line.isVertical)

let solve lines =  
    let board = Board.Empty |> Board.applyLines lines
    board.Positions |> Map.toSeq |> Seq.map snd |> Seq.filter (fun x -> x > 1) |> Seq.length

printfn "Part1: %i" (solve horizontalAndVerticalLines)
printfn "Part2: %i" (solve allLines)