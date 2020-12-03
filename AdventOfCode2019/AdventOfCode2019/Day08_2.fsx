open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day08.txt")

let width = 25
let height = 6
let size = height * width

let line = File.ReadAllLines(file).[0]
let layers = [0 .. line.Length / size - 1] |> List.map (fun i -> line.Substring(i * size, size))

let printPixel = function
    | '0' -> printf " "
    | '1' -> printf "#"
    | '2' -> printf "."
    | _ -> failwith "Invalid case"

let getPixel idx (layers:string list) =
    let coloredPixel = layers |> List.map (fun l -> l.[idx]) |> Seq.tryFind (fun p -> p <> '2')
    match coloredPixel with
    | Some p -> p
    | None -> '2'

for i in [0..size-1] do
    let pixel = layers |> getPixel i
    printPixel pixel
    if ((i+1) % width = 0) then printfn ""