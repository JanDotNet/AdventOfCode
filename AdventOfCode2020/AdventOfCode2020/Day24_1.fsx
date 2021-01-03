open System
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day24.txt")
let input = File.ReadAllLines(file) |> Seq.toList

type Pos = { X:int; Y:int} 
  with static member Empty = { X = 0; Y = 0 } 

let tokanize line =
  let foldTokens (tokenList, stack) = function
    | x when x = "e" || x = "w" -> (stack + x) :: tokenList, ""
    | x -> tokenList, x
  line |> Seq.toList
       |> List.map string
       |> List.fold foldTokens ([], "")
       |> fst |> List.rev

let move pos = function
  | "w" -> {pos with  X = pos.X - 1}
  | "e" -> {pos with  X = pos.X + 1}
  | "ne" -> {pos with Y = pos.Y - 1; X = pos.X + (if pos.Y % 2 = 0 then  0 else 1) }
  | "se" -> {pos with Y = pos.Y + 1; X = pos.X + (if pos.Y % 2 = 0 then  0 else 1) }
  | "nw" -> {pos with Y = pos.Y - 1; X = pos.X + (if pos.Y % 2 = 0 then -1 else 0) }
  | "sw" -> {pos with Y = pos.Y + 1; X = pos.X + (if pos.Y % 2 = 0 then -1 else 0) }
  | x -> failwithf "Invalid direction: %s" x

let result = input 
             |> List.map tokanize
             |> List.map (fun dirs -> dirs |> List.fold move Pos.Empty)
             |> List.groupBy id
             |> List.map (fun (p, l) -> (p, l.Length))
             |> List.filter (fun (_, l) -> l % 2 = 1)
             |> List.length
