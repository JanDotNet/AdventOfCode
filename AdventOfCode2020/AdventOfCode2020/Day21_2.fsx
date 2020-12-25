open System
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day21.txt")
let input = File.ReadAllLines(file) |> Array.toList

type Food = { Ingredients : string list;
              Allergens : string list } with
  static member Parse (line : string) =
    let sl = line.Split([|" (contains "; ")"|], StringSplitOptions.RemoveEmptyEntries)
    { Ingredients = sl.[0].Split(' ') |> Array.toList;
      Allergens = sl.[1].Split([|", "|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList }

let foods = input |> List.map Food.Parse

let ingredientsWithAllergens = foods 
                               |> List.collect (fun food -> food.Allergens |> List.map (fun a -> (a, food.Ingredients |> Set.ofList)))
                               |> List.groupBy fst
                               |> List.map (fun (a, ai) -> (a ,ai |> List.map snd |> Set.intersectMany))

let solve ingredients =
  let rec solve' processed processing = 
    if processing |> List.isEmpty then
      processed
    else
      let processingWithOneElement = processing 
                                     |> List.filter (fun (a, s) -> (s |> Set.count) = 1)
                                     |> List.map (fun (a, s) -> (a, s |> Set.toList |> List.head))
      let processed' = processed @ processingWithOneElement 
      let processedItems = processed' |> List.map snd |> Set.ofList
      let processing' = processing 
                        |> List.filter (fun (a, _) -> processingWithOneElement |> List.exists (fun (a', _) -> a = a') |> not) 
                        |> List.map (fun (a, i) -> (a, Set.difference i processedItems))
      solve' processed' processing'

  (solve' [] ingredients)

let res = ingredientsWithAllergens |> solve |> List.sortBy fst |> List.map snd |> String.concat ","