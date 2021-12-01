open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day01.txt")

//let input = [199;200;208;210;200;207;240;269;260;263]
let input = File.ReadAllLines(file) |> Seq.map int

let solve1 measurements =
    measurements
    |> Seq.pairwise 
    |> Seq.filter (fun (l,r) -> l < r)
    |> Seq.length

let solve2 measurements = 
    measurements
    |> Seq.windowed 3 
    |> Seq.map (fun x -> x.[0] + x.[1] + x.[2] )
    |> Seq.pairwise
    |> Seq.filter (fun (l,r) -> l < r)
    |> Seq.length

printfn "Solution 1: %i" (solve1 input)
printfn "Solution 2: %i" (solve2 input)

