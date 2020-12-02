// Learn more about F# at http://fsharp.org

open System
open MyParser

//type PartialResult = 
//    | SingleCharResult of char
//    | AggregatedResult of string

//let processChar (currentChar : char, aggregate : string) =
//    match currentChar with
//    | 'y' -> ' ' |> SingleCharResult
//    | ' ' -> 'y' |> SingleCharResult
//    | 'a' -> '1' |> SingleCharResult
//    | 'e' -> '2' |> SingleCharResult
//    | 'i' -> '3' |> SingleCharResult
//    | 'o' -> '4' |> SingleCharResult
//    | 'u' -> '5' |> SingleCharResult
//    | x when x |> Char.IsLetter -> x |> int |> (fun x -> x - 1) |> char |> SingleCharResult
//    | x when x |> Char.IsDigit -> (x |> string) + aggregate |> AggregatedResult
//    | _ -> currentChar |> SingleCharResult


//let processInput (input : string) =
//    for c in input do
//        let res = processChar c ""
//        match (res) with
//        | SingleCharResult translatedChar -> 
            
    //input
    //|> Seq.map (fun c->processChar c "")    
    //|> String.Concat


[<EntryPoint>]
let main argv =
    //printfn "%s" ("Hello World from F#!" |> )

    MyParser.pchar 'a'

    0 // return an integer exit code