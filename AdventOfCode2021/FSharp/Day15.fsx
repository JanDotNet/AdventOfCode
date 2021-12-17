open System.IO
open System
open System.Collections
open System.Collections.Generic
open System.Linq


let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day15.txt")

let input2 = "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"

//let input = input2.Split('\n') |> List.ofArray
let input = File.ReadAllLines(file) |> List.ofArray

let parse lines =
    let parseLine (line:string) row =
        line |> Seq.indexed 
             |> Seq.map (fun (i, c) -> (i+1, row), c |> string |> int) 
             |> Seq.toList
    lines |> List.indexed
          |> List.map (fun (i, l) -> parseLine l (i+1)) 
          |> List.concat

let board = input |> parse
let fieldValue = board |> Map.ofList
let maxX = board |> List.map (fst >> fst) |> List.max
let maxY = board |> List.map (fst >> snd) |> List.max
let startPos = (1,1)
let endPos = (maxX, maxY)
let heuristic (x, y) = maxX-x + maxY-y

let getNextPositions (x, y) =
    seq {
        if x < maxX then yield (x+1, y)
        if x > 1 then yield (x-1, y)
        if y < maxY then yield (x, y+1)
        if y > 1 then yield (x, y-1)
    } |> Seq.toList

let mutable queue = PriorityQueue<int*int,int>()
queue.Enqueue(startPos, 0)

let mutable costSoFar:Map<int*int, int> = Map.empty |> Map.add startPos 0
let mutable cameFrom:Map<int*int, int*int> = Map.empty
let mutable finished = false
while queue.Count > 0 && not finished do
    let current = queue.Dequeue()    
    if current = endPos 
    then finished <- false
    else 
        for nextPos in current |> getNextPositions do
            let nextPosCost = costSoFar.[current] + fieldValue.[nextPos]
            let nextPosCostOld = costSoFar.TryFind nextPos
            if nextPosCostOld.IsNone || nextPosCostOld.Value > nextPosCost then
                costSoFar <- costSoFar |> Map.add nextPos nextPosCost
                cameFrom <- cameFrom |> Map.add nextPos current
                let priority  = nextPosCost + (heuristic nextPos)
                queue.Enqueue(nextPos, priority)

let rec getPath path element =
    match element with
    | el when el = startPos -> startPos::path
    | el -> let element' = cameFrom.[element]
            let path' = element::path
            getPath path' element'

let path = getPath [] endPos |> List.map (fun p -> fieldValue.[p]) |> List.sum