open System
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day24.txt")
let input = File.ReadAllLines(file) |> Seq.toList

type Pos = { X:int; Y:int} 
  with static member Empty = { X = 0; Y = 0 } 

let tokenize line =
  let foldTokens (tokenList, stack) = function
    | x when x = "e" || x = "w" -> (stack + x) :: tokenList, ""
    | x -> tokenList, x
  line |> Seq.toList
       |> List.map string
       |> List.fold foldTokens ([], "")
       |> fst |> List.rev

let move pos = function
  | "w"  -> {pos with X = pos.X - 1}
  | "e"  -> {pos with X = pos.X + 1}
  | "ne" -> {pos with Y = pos.Y - 1; X = pos.X + (if pos.Y % 2 = 0 then  0 else 1) }
  | "se" -> {pos with Y = pos.Y + 1; X = pos.X + (if pos.Y % 2 = 0 then  0 else 1) }
  | "nw" -> {pos with Y = pos.Y - 1; X = pos.X + (if pos.Y % 2 = 0 then -1 else 0) }
  | "sw" -> {pos with Y = pos.Y + 1; X = pos.X + (if pos.Y % 2 = 0 then -1 else 0) }
  | x -> failwithf "Invalid direction: %s" x

let getAdjacents pos =
  seq {
    yield {pos with X = pos.X - 1}
    yield {pos with X = pos.X + 1}
    yield {pos with Y = pos.Y - 1; X = pos.X + (if pos.Y % 2 = 0 then  0 else 1) }
    yield {pos with Y = pos.Y + 1; X = pos.X + (if pos.Y % 2 = 0 then  0 else 1) }
    yield {pos with Y = pos.Y - 1; X = pos.X + (if pos.Y % 2 = 0 then -1 else 0) }
    yield {pos with Y = pos.Y + 1; X = pos.X + (if pos.Y % 2 = 0 then -1 else 0) } }

let blackTiles = input 
                 |> List.map tokenize
                 |> List.map (fun dirs -> dirs |> List.fold move Pos.Empty)
                 |> List.groupBy id
                 |> List.filter (fun (p, l) -> l.Length % 2 = 1)
                 |> List.map fst
                 |> Set.ofList

let rec step blackTiles round =
  printfn "Round %i: %i" round (blackTiles |> Set.count)
  
  let whiteTiles' = blackTiles 
                  |> Seq.collect getAdjacents 
                  |> Set.ofSeq
  let whiteTiles = Set.difference whiteTiles' blackTiles

  let countBlackNeigbours p = p |> getAdjacents |> Set.ofSeq |> Set.intersect blackTiles |> Set.count
  
  let fromBlack = blackTiles
                  |> Set.map (fun t -> t, t |> countBlackNeigbours)
                  |> Set.filter (fun (t, c) -> c = 1 || c = 2)
                  |> Set.map fst
  let fromWhite = whiteTiles
                  |> Set.map (fun t -> t, t |> countBlackNeigbours)
                  |> Set.filter (fun (_, c) -> c = 2)
                  |> Set.map fst
  
  (Set.union fromBlack fromWhite)

[0..100] |> List.fold step blackTiles

