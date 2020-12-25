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
                               |> List.map (fun (_, ai) -> ai |> List.map snd |> Set.intersectMany)
                               |> Set.unionMany
                               |> Set.toList

let ingredientsWithoutAllergens = foods |> List.collect (fun f -> f.Ingredients ) |> List.distinct |> List.except ingredientsWithAllergens

let result = foods 
              |> List.collect (fun f -> f.Ingredients) 
              |> List.countBy id 
              |> List.filter (fun (ing, _) -> ingredientsWithoutAllergens |> List.contains ing) 
              |> List.sumBy snd