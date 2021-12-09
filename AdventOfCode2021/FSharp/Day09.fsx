open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day09.txt")

// let intput = "2199943210
// 3987894921
// 9856789892
// 8767896789
// 9899965678".Split('\n') |> List.ofArray

let input = File.ReadAllLines(file) |> List.ofArray

type Pos = { Row:int; Col:int; Val:int } 

let parse (lines:string list) =
    let parseLine (row:int) (line:string) =    
        line |> Seq.indexed |> Seq.map (fun (c, v) -> { Row = row; Col = c; Val = int(string(v)) })    
    lines |> Seq.indexed |> Seq.map (fun (r, line) -> line |> (parseLine r)) |> Seq.concat |> Seq.toList
    

let positions = parse input

let maxRow = positions |> List.map (fun x -> x.Row) |> List.max
let maxCol = positions |> List.map (fun x -> x.Col) |> List.max

let getNeighbourPosiitons (x, y) =
    seq {
        if x > 0 then yield (x-1, y)
        if x < maxRow then yield  (x+1, y)
        if y > 0 then yield  (x, y-1)
        if y < maxCol then yield  (x, y+1)
    } |> Seq.toList
    
let posMap = positions |> List.map (fun x -> (x.Row, x.Col), x.Val) |> Map.ofList

let isLowPoint pos = 
    let neighbours = (pos.Row, pos.Col) |> getNeighbourPosiitons
    neighbours |> List.map (fun x -> posMap.[x]) |> List.forall (fun x -> pos.Val < x)

let rec collectBasins (positions:list<int*int>) (visited:Set<int*int>) (size:int) =    
    if positions.Length = 0 
        then size   
    elif posMap.[positions.Head] = 9 || visited |> Set.contains positions.Head
        then collectBasins positions.Tail visited size
    else
        let neighbours = positions.Head |> getNeighboutPosiitons
        let visited' = visited |> Set.add positions.Head
        let posStack' = positions.Tail @ neighbours
        collectBasins posStack' visited' (size + 1)    

let solve1 p = p |> List.filter (isLowPoint) 
                 |> List.map (fun x -> x.Val + 1) 
                 |> List.sum

let solve2 p = p |> List.filter (isLowPoint) 
                 |> List.map (fun x -> collectBasins [(x.Row, x.Col)] Set.empty 0) 
                 |> List.sortDescending 
                 |> List.take 3 
                 |> List.reduce (*)

printfn "Solution 1: %i" (solve1 positions)
printfn "Solution 2: %i" (solve2 positions)

