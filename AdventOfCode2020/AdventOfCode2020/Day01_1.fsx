open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day01.txt")

let requiredSum = 2020

module Seq =
    let combine lst = seq {
        for (i1, e1) in lst |> Seq.indexed do
            for (i2, e2) in lst |> Seq.indexed do
                if (i1 < i2) then yield (e1, e2) }    
    
let result = File.ReadAllLines(file)
            |> Seq.map int
            |> Seq.combine
            |> Seq.filter (fun (a,b) -> a + b = requiredSum)
            |> Seq.map (fun (a, b) -> a * b)
            |> Seq.tryExactlyOne
    