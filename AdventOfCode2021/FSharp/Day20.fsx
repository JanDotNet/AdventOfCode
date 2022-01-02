open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day20.txt")

let content = "..."
//let input = content.Split('\n') |> List.ofArray
let input = File.ReadAllLines(file) |> List.ofArray

let parseAlgorithm line =
    line |> Seq.map (fun c -> c = '#') |> Seq.indexed |> Map.ofSeq

let parseImage (lines:string list) =
    let parseLine (row:int) (line:string) =
        line |> Seq.indexed |> Seq.map (fun (c, v) -> (c, row), (v = '#'))
    lines |> Seq.indexed |> Seq.map (fun (r, line) -> line |> (parseLine r)) |> Seq.concat |> Map.ofSeq

let algorithm = input |> List.head |> parseAlgorithm
let image = input |> List.skip 2 |> parseImage

let getX = fst >> fst
let getY = fst >> snd
let getValue = snd

let getExtendedPoisitions value image =
    let minX = image |> Seq.map getX |> Seq.min
    let maxX = image |> Seq.map getX |> Seq.max
    let minY = image |> Seq.map getY |> Seq.min
    let maxY = image |> Seq.map getY |> Seq.max
    seq {
        for x in (minX-2)..(maxX+2) do
            for dy in 1..2 do
                yield (x, minY - dy), value
                yield (x, maxY + dy), value
        for y in minY..maxY do
            for dx in 1..2 do
                yield (minX - dx, y), value
                yield (maxX + dx, y), value }


let extentImage value image =
    let extensionPositions = image |> Map.toSeq |> getExtendedPoisitions value |> Map.ofSeq
    Map.fold (fun acc key value -> acc |> Map.add key value) image extensionPositions

let getAlgorithmIndex (x, y) (image:Map<int*int, bool>) =
    seq {
        for dy in [-1;0;1] do
            for dx in [-1;0;1] do
                yield image.[(x+dx, y+dy)]
    } |> Seq.rev |> Seq.indexed |> Seq.map (fun (i, v) -> if v then pown 2 i else 0 ) |> Seq.sum

let processImage extendValue (algo:Map<int,bool>) image =
    let image' = image |> extentImage extendValue
    let minX = (image' |> Map.toSeq |> Seq.map getX |> Seq.min) + 1
    let maxX = (image' |> Map.toSeq |> Seq.map getX |> Seq.max) - 1
    let minY = (image' |> Map.toSeq |> Seq.map getY |> Seq.min) + 1
    let maxY = (image' |> Map.toSeq |> Seq.map getY |> Seq.max) - 1
    seq {   for x in minX .. maxX do
                for y in minY .. maxY do                        
                    let idx = image' |> getAlgorithmIndex (x, y)
                    let value = algo.[idx]
                    (x, y), value } |> Map.ofSeq

let rec processImageN extendValue count (algo:Map<int,bool>) image =
    let extendValue' = if extendValue then algo.[511] else algo.[0]
    let count' = count-1
    match count with
    | 0 -> image
    | _ -> image |> processImage extendValue algo |> processImageN extendValue' count' algo

let countPixels image = image |> Map.toList |> List.map snd |> List.filter id |> List.length

let solve cnt image = image |> processImageN false cnt algorithm |> countPixels

printfn "Solution 01: %i" (image |> solve 2)
printfn "Solution 02: %i" (image |> solve 50)