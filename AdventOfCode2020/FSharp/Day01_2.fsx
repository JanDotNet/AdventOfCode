open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day01.txt")


module Seq =
    let combine lst = seq {
        for (i1, e1) in lst |> Seq.indexed do
            for (i2, e2) in lst |> Seq.indexed do
                for (i3, e3) in lst |> Seq.indexed do
                    if (i1 < i2 && i1 < i3 && i2 < i3) then yield (e1, e2, e3) }    
   

let result = File.ReadAllLines(file)
            |> Seq.map int
            |> Seq.combine
            |> Seq.filter (fun (a,b, c) -> a + b + c = 2020)
            |> Seq.map (fun (a, b, c) -> a * b  * c)
            |> Seq.tryExactlyOne
    