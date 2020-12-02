open System
open System.IO
open System.Globalization

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

type PositionInfo = { Pos: Position list; Distance : int }
    with static member Zero = { Pos = []; Distance = 0}

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

let isBorder pos = pos.X = maxX || pos.X = minX || pos.Y = maxY || pos.Y = minY

let generateAllPositions = 
    seq { for x in minX..maxX do
          for y in minY..maxY do
          yield {X = x; Y = y } }

let distance p1 p2 =
    let x = (p1.X - p2.X) |> abs
    let y = (p1.Y - p2.Y) |> abs
    x + y

let getDistanceInfo posToCheck =
    let folder (posInfo : PositionInfo) (pos : Position) =
        let dis = distance pos posToCheck
        match posInfo.Pos with
            | [] -> { Pos = [pos]; Distance = dis }
            | _ when posInfo.Distance < dis -> posInfo
            | _ when posInfo.Distance = dis -> { posInfo with Pos = pos :: posInfo.Pos }
            | _ when posInfo.Distance > dis -> { Pos = [pos]; Distance = dis }
            | _ -> failwith "should not happen"
    positions |> List.fold folder PositionInfo.Zero
    
let all = generateAllPositions 
        |> Seq.map (fun p -> (p, p |> getDistanceInfo))
        |> Seq.filter (fun (a, b) -> b.Pos.Length = 1)
        |> Seq.map (fun (a, b) -> (a, b.Pos.Head))
        |> Seq.toList

let infinity = all 
            |> List.filter (fun (p, pi) -> p |> isBorder) 
            |> List.map (fun (p, pi) -> pi)
            |> List.distinct

let nonInfinity = all
                   |> List.filter (fun (p, pi) -> infinity |> List.contains pi |> not)
                   |> List.groupBy (fun (p, pi) -> pi)
                   |> List.map (fun (a, b) -> (a, b.Length))
                   |> List.sortByDescending (fun (a, b) -> b)