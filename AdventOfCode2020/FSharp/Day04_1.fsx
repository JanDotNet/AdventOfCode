open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day04.txt")

let preprocessInput (agg, lst) = function
                                | "" -> ( "", agg :: lst)
                                | item -> ( String.Join(" ", [agg; item]), lst)

let lineToMap (line:string) = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) 
                                      |> Seq.map (fun i -> i.Split(':'))
                                      |> Seq.map (fun s -> s.[0], s.[1])
                                      |> Map.ofSeq
let required = ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"] //;"cid"

let result = File.ReadAllLines(file) 
              |> Seq.fold preprocessInput ("", []) 
              |> snd
              |> Seq.map lineToMap
              |> Seq.filter (fun m -> required |> List.forall m.ContainsKey)
              |> Seq.length
