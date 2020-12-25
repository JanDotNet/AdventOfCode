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
let mutable counter = 1

let getFinalDeck deck1 deck2 =  
  let rec getFinalDeck' d1 d2 processed level step =
      let processed' = processed |> Set.add (d1, d2)
      printfn "Level: %i Step: %i" level step  
      match (d1, d2) with
      | x when processed |> Set.contains x -> (1, d1)
      | ([], rem2) -> (2, rem2)
      | (rem1, []) -> (1, rem1)
      | (h1::rem1, h2::rem2) when h1 <= rem1.Length && h2 <= rem2.Length 
          -> let (p, _) = getFinalDeck' (rem1 |> List.take h1) (rem2 |> List.take h2) processed' (level+1) 1
             if p = 1 
             then getFinalDeck' (rem1 @ [h1;h2]) rem2 processed' level (step+1)
             else getFinalDeck' rem1 (rem2 @ [h2;h1]) processed' level (step+1)
      | (h1::rem1, h2::rem2) when h1 > h2 
        -> getFinalDeck' (rem1 @ [h1;h2]) rem2 processed' level (step+1)
      | (h1::rem1, h2::rem2) when h2 > h1 
        -> getFinalDeck' rem1 (rem2 @ [h2;h1]) processed' level (step+1)
      | _ -> failwith "should not happen"
  getFinalDeck' deck1 deck2 Set.empty 0 1

let finalDeck = getFinalDeck p1 p2
let result = finalDeck |> snd |> List.rev |> List.mapi (fun i e -> (i+1)*e) |> List.sum