#r "../packages/FSharp.Data/lib/netstandard2.0/FSharp.Data.dll"

open System
open System.IO
open FSharp.Data
open System.Collections.Generic

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_02.txt"
let input = File.ReadAllLines(inputFile) |> Seq.toList;

let input2 = [
        "abcdef";
        "bababc";
        "abbcde";
        "abcccd";
        "aabcdd";
        "abcdee";
        "ababab";
    ]

let addTuple (a, b) (c, d) = (a + c, b + d)

let listToTuple lst =
    match lst with
        | [2; 3] -> (1, 1)   
        | [2] -> (1, 0)
        | [3] -> (0, 1)
        | _ -> (0, 0)

let itemToList lst =
    lst
    |> Seq.toList 
    |> List.groupBy (fun x -> x) 
    |> List.map (fun (i, l) -> l.Length)
    |> List.filter (fun c -> c = 2 || c = 3)
    |> List.sortBy (fun c -> c)
    |> List.distinct

let result = 
    input 
    |> List.map itemToList 
    |> List.map listToTuple 
    |> List.reduce addTuple
    |> (fun (a, b) -> a * b)

printfn "RESULT: %i" result
    