open System.IO
open System
open System.Text.RegularExpressions

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day03.txt")

module Seq = let rec cycle xs = seq { yield! xs; yield! cycle xs }

let lines = File.ReadAllLines(file)

let xRightOneDown lst x = lst |> Seq.map Seq.cycle
                              |> Seq.indexed
                              |> Seq.map (fun (i, row) -> row |> Seq.item (i * x))
                              |> Seq.filter (fun x -> x = '#')
                              |> Seq.length

let OneRightxDown lst x = lst |> Seq.map Seq.cycle
                              |> Seq.indexed
                              |> Seq.filter (fun (i, row) -> i <> 0 && i % 2 = 0)
                              |> Seq.map (snd)
                              |> Seq.indexed
                              |> Seq.map (fun (i, row) -> row |> Seq.item (i + 1))
                              |> Seq.filter (fun x -> x = '#')
                              |> Seq.length

let results = ([1; 3; 5; 7] |> Seq.map (xRightOneDown lines) |> Seq.toList) @
              ([2] |> Seq.map (OneRightxDown lines) |> Seq.toList)

let result = results |> List.map int64 |> List.reduce (*)