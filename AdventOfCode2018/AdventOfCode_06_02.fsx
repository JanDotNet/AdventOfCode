open System
open System.IO

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_06.txt"
let input = File.ReadAllLines(inputFile) |> Seq.toList;

let input2 = [
    "1, 1"
    "1, 6"
    "8, 3"
    "3, 4"
    "5, 5"
    "8, 9"]

type Position = { X:int ; Y:int }

let parse (str:string) =
    let lst = str.Split([|','; ' '|], StringSplitOptions.RemoveEmptyEntries) 
              |> Array.map int
    { X = lst.[0]; Y = lst.[1] }

let positions = input |> List.map parse

let get agg selector lst = lst |> List.map selector |> List.reduce agg
let maxX = positions |> get max (fun p -> p.X)
let maxY = positions |> get max (fun p -> p.Y)
let minX = positions |> get min (fun p -> p.X)
let minY = positions |> get min (fun p -> p.Y)

let generateAllPositions = 
    seq { for x in minX..maxX do
          for y in minY..maxY do
          yield {X = x; Y = y } }

let distance p1 p2 =
    let x = (p1.X - p2.X) |> abs
    let y = (p1.Y - p2.Y) |> abs
    x + y

let getTotalDistance posToCheck =
    let folder (totalDistance : int) (pos : Position) =
        let dis = distance pos posToCheck
        totalDistance + dis
    positions |> List.fold folder 0
    
let all = generateAllPositions 
        |> Seq.map (fun p -> (p, p |> getTotalDistance))
        |> Seq.filter (fun (a, b) -> b < 10000)
        |> Seq.length