open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day07.txt")

//let input = "16,1,2,0,4,2,7,1,2,14"

let input = File.ReadAllLines(file) |> Array.head

let crap_positions = input.Split(',') |> Seq.map int64 |> Seq.toList

let get_fuel_consumption distance_transformation target_pos = 
    let fuels_cost pos = (pos - target_pos) |> abs |> distance_transformation
    crap_positions |> List.map fuels_cost |> List.sum

let max_pos = crap_positions |> List.max
let min_pos = crap_positions |> List.min
let target_positions = [min_pos .. max_pos]

let solve distance_transformation = 
    target_positions 
        |> List.map (get_fuel_consumption distance_transformation) 
        |> List.min

printfn "Part1: %i" (solve id)
printfn "Part2: %i" (solve (fun n -> [1L..n] |> List.sum))
