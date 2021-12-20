open System
open System.IO
open System.Collections.Generic

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day18.txt")

let input = File.ReadAllLines(file) |> Array.toList

type Node =
    | Value of int
    | Pair of Node * Node

type ExplodeState =
    | NotFound
    | Found of int * int
    | LeftProcessed of int
    | RightProcessed of int
    | Completed

let print node =
    let toPositions node =
        let rec toPosition' depth col node =
            match node with
            | Value (x) -> col+1, [(col+1, depth, x|> string)]
            | Pair (l, r) -> let col', positions' = (l |> toPosition' (depth+1) col)
                             let node' = (col'+1,depth, "^")
                             let col'', positions'' = (r |> toPosition' (depth+1) (col'+1))
                             col'', positions'@(node'::positions'')
        toPosition' 0 0 node |> snd
    let positions = node |> toPositions
    let maxX = positions |> List.map (fun (x,_,_) -> x) |> List.max
    let maxY = positions |> List.map (fun (_,x,_) -> x) |> List.max
    let map = positions |> List.map (fun (x, y, v) -> (x, y), v) |> Map.ofList
    for y in 0..maxY do
        printf "%i:" y
        for x in 1..maxX do
            printf "%s" (match map.TryFind (x, y) with Some(s) -> s | None -> " ")
        printfn ""
        



let toInt = string >> int

let parse (line:string) =
    let stream = line |> List.ofSeq
    let rec getElements (stack:Node list) stream =
        match stream, stack with
        | [], _ -> stack |> List.head
        | '['::stream', _ 
            -> stream' |> getElements stack
        | x::stream', _ when Char.IsDigit x 
            -> stream' |> getElements (Value(x |> toInt)::stack)
        | ','::stream', _ 
            -> stream' |> getElements stack
        | ']'::stream', r::l::stack' 
            -> let stack'' = Pair(l, r)::stack'
               stream' |> getElements stack''
        | _ -> failwith "should not happen"
    stream |> getElements []

let explodeFirst node =
    let rec addLeftSide valueToAdd node =
        match node with
        | Value (x) -> Value (x + valueToAdd)
        | Pair (l, r) -> Pair (l |> addLeftSide valueToAdd, r)

    let rec addRightSide valueToAdd node =
        match node with
        | Value (x) -> Value (x + valueToAdd)
        | Pair (l, r) -> Pair(l, r |> addRightSide valueToAdd)

    let rec explode depth (node:Node) =
        match node with
        | Value(_) -> NotFound, node
        | Pair(Value(vl), Value(vr)) when depth >= 4 -> Found (vl, vr), node
        | Pair(l, r) -> let stateLeft, l' = l |> explode (depth+1)         
                        match stateLeft with     
                        | Completed -> Completed, Pair(l', r)                        
                        | Found (vl, vr) -> RightProcessed(vl), Pair( Value(0),r |> addLeftSide vr)                          
                        | RightProcessed (vl) -> if depth = 0 
                                                 then Completed, Pair(l', r)
                                                 else RightProcessed (vl), Pair(l', r)
                        | LeftProcessed (vr) -> Completed, Pair(l', r |> addLeftSide vr)
                        | NotFound -> let stateRight, r' = r |> explode (depth+1)                                      
                                      match stateRight with
                                      | Completed -> Completed, Pair(l, r')
                                      | Found (vl, vr) -> LeftProcessed(vr), Pair(l |> addRightSide vl, Value(0))
                                      | LeftProcessed (vr) -> if depth = 0 
                                                              then Completed, Pair(l, r')
                                                              else LeftProcessed (vr), Pair(l, r')
                                      | RightProcessed (vl) -> Completed, Pair(l |> addRightSide vl, r')
                                      | NotFound -> NotFound, node

        
    explode 0 node 

let rec linearize = function
    | Pair(l, r) -> (l |> linearize) @ (r |> linearize)
    | Value(x) -> [Value(x)]
 
let rec splitFirst (node:Node) =
    let split x = let l = Value (Math.Floor (float(x)/2.0) |> int)
                  let r = Value (Math.Ceiling (float(x)/2.0) |> int)
                  l, r
    let rec splitFirst' (hasSplitted:bool) node =    
        match node with
        | Pair(l, r) -> let hasSplitted', left = l |> splitFirst' hasSplitted
                        let hasSplitted'', right = r |> splitFirst' hasSplitted'
                        hasSplitted'', Pair (left, right)
        | Value (x) when x > 9 && (not hasSplitted) -> true, Pair (split x)
        | Value (x) -> hasSplitted, Value (x)
    node |> (splitFirst' false)

let rec calcMagnitude node =
    match node with
    | Value(x) -> x
    | Pair(l, r) -> 3 * calcMagnitude l + 2 * calcMagnitude r

"[[1,2],[[3,4],5]]" |> parse |> calcMagnitude

let rec reduceNode node =
    let rec reduceExplode node =
        let state, node' = node |> explodeFirst
        match state with
        | NotFound -> node'
        | _ -> reduceExplode node'

    let node' = node |> reduceExplode
    let splitted, node'' = node' |> splitFirst
    if splitted then reduceNode node'' else node''

let rec doHomework nodes =
    let folder state node =
        match state with 
        | None -> Some(node |> reduceNode)
        | Some (n) -> Some(Pair(n, node) |> reduceNode)
    nodes |> List.fold folder None

let rec combineAllElements list =
    match list with
    | [] -> []
    | h::t -> let list' = seq {
                    for e in t do
                        yield (h, e)
                        yield (e, h) } |> Seq.toList
              let list'' = t|> combineAllElements
              list' @ list''

let solve1 input = input 
                    |> List.map parse 
                    |> doHomework 
                    |> Option.map calcMagnitude

let solve2 input = input
                    |> List.map parse
                    |> combineAllElements
                    |> List.map (fun (a, b) -> Pair(a, b) |> reduceNode |> calcMagnitude)
                    |> List.max

printfn "Solution 1: %O " (input |> solve1)
printfn "Solution 2: %i " (input |> solve2)
