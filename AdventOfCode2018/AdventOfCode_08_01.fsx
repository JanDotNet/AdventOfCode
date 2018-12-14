open System
open System.IO
open System.Globalization

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_08.txt"
let input = File.ReadAllLines(inputFile) |> Seq.head;
let numbers = input.Split([|' '|]) |> Array.toList |> List.map int

let rec sumMetadata lstin =
    let rec loop sum lst =
        match lst with
        |  0::mc::tail -> ((tail |> List.take mc |> List.sum) + sum), (tail |> List.skip mc)
        | cc::mc::tail -> 
            let sum', tail' = loop sum tail
            loop sum' ((cc - 1)::mc::tail')
        | [x] -> failwith "should not happen"
        | []  -> failwith "should not happen"
    loop 0 lstin

let a = sumMetadata numbers