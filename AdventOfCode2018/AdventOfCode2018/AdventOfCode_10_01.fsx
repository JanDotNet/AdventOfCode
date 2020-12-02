open System
open System.IO
open System.Globalization

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_10.txt"
let input = File.ReadAllLines(inputFile) |> Array.toList

type Item = { PosX:int; PosY:int; VelX:int; VelY:int }

let parse (line:string) =
    {
        PosX = line.Substring(10,6) |> int;
        PosY = line.Substring(17,7) |> int;
        VelX = line.Substring(36, 2) |> int;
        VelY = line.Substring(39, 3) |> int;
    }

let move item =
    { item with 
        PosX = item.PosX + item.VelX; 
        PosY = item.PosY + item.VelY }

let getBorder items =
    let rec loop xMin xMax yMin yMax lst =
        match lst with
        | h::tail -> loop (min xMin h.PosX) (max xMax h.PosX) (min yMin h.PosY) (max yMax h.PosY) tail
        | []     -> (xMin, yMin)
    loop 999999 -999999 9999 -9999 items

let items = input |> List.map parse


let rec printItems lst round =
    let n = lst |> List.map move
    if round = 10085 then
        let xMin, yMin = n |> getBorder
        for y in [yMin..(yMin+9)] do
            for x in [xMin..(xMin+61)] do
                printf "%s" (n |> List.tryFind (fun i -> i.PosX = x && i.PosY = y) |>  (fun i -> if Option.isSome i then "X" else " "))
            printfn ""
    else
        printItems n (round + 1)


printItems items 0