open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day09.txt")
let numbers = File.ReadAllLines(file) |> Array.toList |> List.map int64

let preambleSize = 25

let sumPairs l 
    = (List.allPairs l l) |> List.map (fun (x, y) -> x + y) |> List.distinct

let preambles = numbers |> List.take (numbers.Length - 1) |> List.windowed preambleSize |> List.map sumPairs
let numbersToCheck = numbers |> List.skip preambleSize
let preambleNumberPairs = List.zip numbersToCheck preambles
let result = preambleNumberPairs |> List.filter (fun (num, lst) -> lst |> List.contains num |> not) |> List.map fst