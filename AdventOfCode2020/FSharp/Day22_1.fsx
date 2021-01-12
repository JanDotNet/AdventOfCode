open System
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day22.txt")
let input = File.ReadAllLines(file) |> Array.toList

let getDeck playerNo i = i |> List.skipWhile (fun l -> l <> (sprintf "Player %i:" playerNo))  
                           |> List.skip 1 
                           |> List.takeWhile (fun l -> l <> "") 
                           |> List.map int
let p1 = input |> getDeck 1
let p2 = input |> getDeck 2

let rec getFinalDeck d1 d2 =
  match (d1, d2) with
  | ([], d2) -> d2
  | (d1, []) -> d1
  | (h1::d1, h2::d2) when h1 > h2 -> getFinalDeck (d1 @ [h1;h2]) d2
  | (h1::d1, h2::d2) when h2 > h1 -> getFinalDeck d1 (d2 @ [h2;h1])
  | _ -> failwith "should not happen"

let finalDeck = getFinalDeck p1 p2
let result = finalDeck |> List.rev |> List.mapi (fun i e -> (i+1)*e) |> List.sum