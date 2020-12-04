open System.IO
open System
open System.Text.RegularExpressions

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day04.txt")

let folder (agg, lst) = function
                      | "" -> ( "", agg :: lst)
                      | item -> ( String.Join(" ", [agg; item]), lst)

let lineToKeyValueMap (line:string) = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) 
                                      |> Seq.map (fun i -> i.Split(':'))
                                      |> Seq.map (fun s -> s.[0], s.[1])
                                      |> Map.ofSeq
let required = ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"] //;"cid"

let result = File.ReadAllLines(file) 
              |> Seq.fold folder ("", []) 
              |> snd
              |> Seq.map lineToKeyValueMap
              |> Seq.filter (fun m -> required |> List.forall m.ContainsKey)
              |> Seq.length
