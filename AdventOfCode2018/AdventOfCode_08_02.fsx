open System.IO

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_08.txt"
let input = File.ReadAllLines(inputFile) |> Seq.head;
let numbers = input.Split([|' '|]) |> Array.toList |> List.map int

type Node =
    | Leaf of int list
    | Branch of int list * Node list

let rec foldNode (childList:Node list) remaining =
        match remaining with
        |  0::mc::tail ->
            let metadata    = tail |> List.take mc
            let tail'       = tail |> List.skip mc
            let node        = if childList.IsEmpty then Leaf(metadata) else Branch(metadata, childList)
            (node, tail')
        | cc::mc::tail ->
            let node, remaining' = foldNode [] tail
            foldNode (childList @ [node]) ((cc-1)::mc::remaining')

let rec calculateValue node = 
    match node with
    | Leaf md           -> md |> List.sum
    | Branch (md, c)    ->
        let indeces = md |> List.filter (fun x -> x <= c.Length && x > 0) |>  List.map (fun x -> x - 1)
        let childs = c |> List.toArray
        indeces |> List.map (fun idx -> childs.[idx] |> calculateValue) |> List.fold (+) 0

let (node, rest) = foldNode [] numbers
let result = calculateValue node