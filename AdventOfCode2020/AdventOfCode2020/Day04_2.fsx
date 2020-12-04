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

let isBetween min max value = value <= max && value >= min
let isHeightValid (value:string) = 
                    if value.EndsWith("cm")  
                    then value.Substring(0, value.Length-2) |> int |> isBetween 150 193
                    elif value.EndsWith("in")
                    then value.Substring(0, value.Length-2) |> int |> isBetween 59 76
                    else false
let isEyeColorValid v = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> List.contains v
let isHairColorValid v = Regex.IsMatch(v, "^#[0-9a-f]{6}$")
let isPassportValid v = Regex.IsMatch(v, "^0*[0-9]{9}$")

let isValid = function
              | ("byr", value) -> (value |> int) |> isBetween 1920 2002
              | ("iyr", value) -> (value |> int) |> isBetween 2010 2020
              | ("eyr", value) -> (value |> int) |> isBetween 2020 2030
              | ("hgt", value) -> value |> isHeightValid
              | ("hcl", value) -> value |> isHairColorValid
              | ("ecl", value) -> value |> isEyeColorValid
              | ("pid", value) -> value |> isPassportValid
              | _ -> true

let result = File.ReadAllLines(file) 
              |> Seq.fold folder ("", []) 
              |> snd
              |> Seq.map lineToKeyValueMap
              |> Seq.filter (fun m -> required |> List.forall m.ContainsKey)
              |> Seq.map (fun m -> m |> Map.toList)
              |> Seq.filter (fun l -> l |> Seq.forall isValid)
              |> Seq.length
