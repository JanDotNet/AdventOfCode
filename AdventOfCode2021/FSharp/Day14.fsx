open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day14.txt")

let input2 = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

//let input = input2.Split('\n') |> List.ofArray
let input = File.ReadAllLines(file) |> List.ofArray

let parse (input:string list) =
    let parsePairInsertions (line:string) =
        let splitted = line.Split(" -> ")
        (splitted.[0].[0], splitted.[0].[1]), (splitted.[1].[0])
    let template = input |> List.head |> Seq.toList
    let pairInsertions = input |> List.skip 2 |> List.map parsePairInsertions |> Map.ofList
    template, pairInsertions

let template, pairInsertions = input |> parse

let templatePairs = template |> List.pairwise
let templateMap = pairInsertions |> Map.map (fun k _ -> templatePairs |> List.filter (fun p -> p = k) |> List.length |> int64)
let templateElementsCount = template 
                            |> Seq.ofList 
                            |> Seq.groupBy id 
                            |> Seq.map (fun (k, lst) -> k, lst |> Seq.length |> int64) 
                            |> Seq.toList

let getNextPairs ((l, r), count) =
    let ins = pairInsertions.[(l, r)]
    [(l, ins), count; (ins, r), count], (ins, count)

let rec step (stepNo:int) (elementsCount:(char*int64) list) (map:Map<char*char, int64>) =
    (* Functions *)
    let sumSnd (k, lst) = k, lst |> List.map snd |> List.sum
    (* Logic *)
    let nextPairsElementCount   = map |> Map.toList |> List.map getNextPairs
    let nextPairs               = nextPairsElementCount |> List.map fst |> List.concat
    let insertedElementsCount   = nextPairsElementCount |> List.map snd    
    let map'                    = nextPairs |> List.groupBy fst |> List.map sumSnd |> Map.ofList
    let elementCount'           =  (insertedElementsCount @ elementsCount) |> List.groupBy fst|> List.map sumSnd

    if stepNo = 1 
    then map', elementCount'
    else step (stepNo-1) elementCount' map'

let solve steps =
    let counts = templateMap |> step steps templateElementsCount |> snd |> List.map snd
    let min = counts |> List.min
    let max = counts |> List.max
    max - min

printfn "Solution 1: %i" (solve 10)
printfn "Solution 2: %i" (solve 40)
