open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day13.txt")

let input2 = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"

//let input = input2.Split('\n') |> List.ofArray

let input = File.ReadAllLines(file) |> List.ofArray


let parse input =
    let parseCoordinate (line:string) =
        let splitted = line.Split(',')
        (splitted.[0] |> int), (splitted.[1] |> int)
    let parseFoldInstruction (line:string) =
        let splitted = line.Split(' ')
        let splitted' = splitted.[2].Split('=')
        splitted'.[0], (splitted'.[1] |> int)
    let firstPart = input |> List.takeWhile(fun x -> x <> "")
    let secondPart = input |> List.skipWhile(fun x -> x.StartsWith("fold") |> not)
    let coordinates = firstPart |> List.map parseCoordinate
    let foldInstruction = secondPart |> List.map parseFoldInstruction
    coordinates, foldInstruction

let applyFoldInstruction foldInstruction coordinates =
    let apply (x, y) =
        match foldInstruction with
        | ("y", v) when y > v -> (x, 2*v-y)
        | ("x", v) when x > v -> (2*v-x, y)
        | _ -> (x, y)
    coordinates |> List.map apply

let rec applyFoldInstructions foldInstructions coordinates =
    match foldInstructions with
    | [] -> coordinates
    | h::t -> applyFoldInstructions t (coordinates |> applyFoldInstruction h)

let printCoordinates coordinates =
    let maxX = coordinates |> List.map fst |> List.max    
    let maxY = coordinates |> List.map snd |> List.max
    for x in [0..maxX] do
        for y in [0..maxY] do
            printf (if coordinates |> List.contains (x, y) then "#" else ".")
        printfn ""

let coordinates, foldInstructions = input |> parse
let foldInstruction = foldInstructions |> List.head

coordinates |> applyFoldInstruction foldInstruction
            |> List.distinct
            |> List.length

coordinates |> applyFoldInstructions foldInstructions
            |> List.distinct
            |> printCoordinates

        

// printfn "Solution 1: %i" (input |> solve1)
// printfn "Solution 2: %i" (input |> solve2)