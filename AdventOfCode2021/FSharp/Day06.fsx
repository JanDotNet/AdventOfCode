open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day06.txt")

//let input = "3,4,3,1,2"

let input = File.ReadAllLines(file) |> Array.head

let startPopulation = input.Split(',') |> Seq.map int |> Seq.toList

let folder (population:int64 array) day =
    let spawnCount = population.[0]
    for i in 0 .. 7 do
        population.[i] <- population.[i+1]
    population.[8] <- spawnCount
    population.[6] <- population.[6] + spawnCount
    population

let calcForStartValue start days =
    let startPopulation = [0 .. 8] |> List.map (fun x -> if start = x then 1L else 0L) |> List.toArray
    [ 1 .. days ] |> List.fold folder startPopulation |> Array.sum

let solve days = 
    let preCalculation = [1;2;3;4;5] |> List.map (fun x -> (x, calcForStartValue x days)) |> Map.ofList
    startPopulation |> List.map (fun x -> preCalculation.[x]) |> List.sum

printfn "Part1: %i" (solve 80)
printfn "Part2: %i" (solve 256)
