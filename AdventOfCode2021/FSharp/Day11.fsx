open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day11.txt")

// let input = "5483143223
// 2745854711
// 5264556173
// 6141336146
// 6357385478
// 4167524645
// 2176841721
// 6882881134
// 4846848554
// 5283751526".Split('\n') |> List.ofArray

let input = File.ReadAllLines(file) |> List.ofArray

let parse lines =
    let parseLine row line =
        line |> Seq.indexed |> Seq.map (fun (i, c) -> (row, (i)), int(string(c)))
    lines |> List.indexed |> List.map (fun (i, l) -> l |> parseLine (i)) |> Seq.concat |> Map.ofSeq

let posMap = input |> parse

let getX = fst >> fst
let getY = fst >> snd
let maxX = posMap |> Map.toList |> List.map getX |> List.max
let maxY = posMap |> Map.toList |> List.map getY |> List.max

let getNeighbours (x, y) =
    seq {
        if x > 0                then yield x-1, y
        if x < maxX             then yield x+1, y
        if y > 0                then yield x, y-1
        if y < maxY             then yield x, y+1
        if x > 0 && y > 0       then yield x-1, y-1
        if x < maxX && y > 0    then yield x+1, y-1
        if x < maxX && y < maxY then yield x+1, y+1
        if x > 0 && y < maxY    then yield x-1, y+1
    } |> Seq.toList

let rec incrementPositions (positions:(int*int) list) (map:Map<(int*int), int>) =
    match positions with
    | []   -> map
    | h::t -> map |> Map.add h (map.[h]+1) 
                  |> incrementPositions t

let incrementAllPositions (map:Map<(int*int), int>) = 
    let allPositions = map |> Map.toList |> List.map fst
    incrementPositions allPositions map

let resetPosition pos (map:Map<(int*int), int>) = 
    map |> Map.add pos 0

let getFlashPositions (posMap:Map<(int*int), int>) =
    posMap |> Map.toList |> List.filter (fun (p, v) -> v > 9) |> List.map fst

let incrementFlashes map =
    let rec incrementNextFlash flashCount map =
        let pos = map |> Map.tryFindKey (fun p v -> v > 9)
        match pos with        
        | None -> map, flashCount
        | Some (pos) -> let neigbours = pos |> getNeighbours |> List.filter (fun p -> map.[p] <> 0)
                        map |> incrementPositions neigbours
                            |> resetPosition pos
                            |> incrementNextFlash (flashCount + 1)                    
    map |> incrementNextFlash 0

let solve1 map =
    let rec applySteps steps total map =        
        if steps = 0 then total
        else map |> incrementAllPositions
                |> incrementFlashes
                |> (fun (m, t) -> m |> applySteps (steps-1) (total + t))
    map |> applySteps 100 0

let solve2 map =
    let rec applySteps steps total map =
        if total = 100 then steps
        else map |> incrementAllPositions
                |> incrementFlashes
                |> (fun (m, t) -> m |> applySteps (steps+1) t)
    map |> applySteps 0 0

printfn "Solution1: %i" (posMap |> solve1)
printfn "Solution2: %i" (posMap |> solve2)

