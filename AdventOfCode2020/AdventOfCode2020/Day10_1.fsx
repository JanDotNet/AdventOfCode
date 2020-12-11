open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day10.txt")
let numbers = File.ReadAllLines(file) |> Array.toList |> List.map int

let result = numbers 
                |> List.sort 
                |> List.pairwise 
                |> List.map (fun (l, r) -> r - l)
                |> List.countBy id
                |> List.map (fun (_, c) -> c+1)
                |> List.reduce (*)