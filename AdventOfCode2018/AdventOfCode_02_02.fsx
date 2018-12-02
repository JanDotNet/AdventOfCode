open System
open System.IO

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_02.txt"
let input = File.ReadAllLines(inputFile) |> Seq.toList;

let compare (item1:string, item2:string) =
    let rec compareInner matchSoFar notMatchingCount = function
        | ([], [])          ->  match (matchSoFar, notMatchingCount) with
                                    | (x, 1) -> Some x 
                                    | _      -> None
        | ([], _) | (_, []) -> failwith "item1 and item2 must have the same size"
        | (h1::t1, h2::t2)  -> match (h1 = h2) with
                               | true  ->   compareInner (matchSoFar @ [h1]) notMatchingCount (t1, t2)
                               | false ->   compareInner matchSoFar (notMatchingCount+1) (t1, t2)

    let c1 = item1 |> Seq.toList
    let c2 = item2 |> Seq.toList
    compareInner [] 0 (c1, c2)

let rec combine = function
    | [] -> []
    | h::t -> (t |> List.allPairs [h]) |> List.append (combine t)

input
|> combine 
|> List.map compare 
|> List.choose id
|> List.map (fun x -> new String(x |> List.toArray))
|> List.iter (printfn "%s")