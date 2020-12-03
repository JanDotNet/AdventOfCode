open System.IO
open System
open System.Text.RegularExpressions

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day03d.txt")

module Seq = let rec cycle xs = seq { yield! xs; yield! cycle xs }

let result = File.ReadAllLines(file)
            |> Seq.map Seq.cycle
            |> Seq.indexed
            |> Seq.map (fun (i, row) -> row |> Seq.item (i * 3))
            |> Seq.filter (fun x -> x = '#')
            |> Seq.length