open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day10.txt")
let numbers = File.ReadAllLines(file) |> Array.toList |> List.map (int)

let countAdjustedOnes (lst, agg) = function
    | 1 -> (lst, agg + 1)
    | 3 -> (agg :: lst, 0)
    | _ -> failwith("invalid case")

let getPossibleVariations num =  
    [|0;1;2;4;7|].[num-1]

let result = (0 :: numbers)
                |> List.sort 
                |> List.pairwise 
                |> List.map (fun (l, r) -> r - l)
                |> List.fold countAdjustedOnes ([], 0)
                |> (fun v -> (snd v) :: (fst v))
                |> List.map (fun x -> x + 1)
                |> List.map getPossibleVariations
                |> List.filter (fun x -> x > 0)
                |> List.map bigint
                |> List.reduce (*)