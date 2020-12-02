open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day06.txt")

let input = File.ReadAllLines(file) 
            |> Array.map (fun x -> x.Split(')'))
            |> Array.map (fun x -> (x.[0], x.[1]))
            |> Array.toList

let parentChildMap = input  
                    |> List.groupBy fst
                    |> List.map (fun (k, l) -> (k, l |> List.map snd))
                    |> Map.ofList
                    
type Orbit =
    | Node of string * Orbit list
    | Leaf of string

let rec toObj (map:Map<string, string list>) value =
    match map.ContainsKey value with
    | true -> Node(value, (map |> Map.find value) |> List.map (toObj map))
    | false -> Leaf(value)

let root = "COM" |> (toObj parentChildMap)

let rec solvePart1 obj depth =
    match obj with
    | Node (_, children) -> (children |> List.sumBy (fun x -> (solvePart1 x (depth + 1)))) + depth
    | Leaf _ -> depth

solvePart1 root 0

let findPath target node =
    let rec findPath' path = function        
        | Node (name, _) when name = target -> Some(path)
        | Node (name, children) -> children |> List.tryPick (findPath' (name :: path))
        | Leaf (name) when name = target -> Some(path)
        | Leaf (_) -> None
    node |> findPath' []

let rec solvePart2 root =
    let pathSan = root |> findPath "SAN" |> Option.map Set.ofList
    let pathYou = root |> findPath "YOU" |> Option.map Set.ofList
    match (pathSan, pathYou) with
        | Some (x), Some(y) -> Some ((x - y) + (y - x) |> Set.count)
        | _ -> None

solvePart2 root
