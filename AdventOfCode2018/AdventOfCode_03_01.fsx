open System
open System.IO

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_03.txt"
let input = File.ReadAllLines(inputFile) |> Seq.toList;

type claim = { x : int; y : int; width : int; height : int }

let getSize c = (c.x + c.width - 1, c.y + c.height - 1)
let hit (x, y) c =
        x >= c.x && x < (c.x + c.width) &&
        y >= c.y && y < (c.y + c.height)

let parse (claimStr:string) =
    let s = claimStr.Split([|'#'; ' '; '@'; ','; ':'; 'x'|], StringSplitOptions.RemoveEmptyEntries) 
            |> Array.skip 1 
            |> Array.map int    
    { x = s.[0]; y = s.[1]; width = s.[2]; height = s.[3] }

let max a b = if a > b then a else b

let expand (a1, a2) (b1, b2) =
    (max a1 b1, max a2 b2)

let rec factorySize claims currentMax =
    match claims with
    | []    -> currentMax
    | h::t  -> h |> getSize |> expand currentMax |> factorySize t

let iteratePositions (xMax, yMax) =
    let getNextPosition = function
        | (x, y) when x = xMax && y = yMax  -> None
        | (x, y) when x = xMax              -> Some (0, y + 1)
        | (x, y)                            -> Some (x + 1, y)
    let rec iterate pos = seq {
        yield pos 
        let next = getNextPosition pos
        if Option.isSome next then yield! iterate (next |> Option.get) }
    iterate (0, 0)
     
let claims = input |> List.map parse
let facSize = factorySize claims (0, 0)

let rec checkPos claimLst count pos =
    match (claimLst, count) with
    | ( _, 2) -> true
    | ([], cnt) -> false
    | (h::t, cnt) -> checkPos t (if (h |> hit pos) then cnt + 1 else cnt) pos
    
let count = iteratePositions facSize
            |> Seq.map (checkPos claims 0)
            |> Seq.filter id
            |> Seq.length

printfn "%i" count