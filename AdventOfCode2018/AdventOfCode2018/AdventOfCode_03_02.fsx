open System
open System.IO

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_03.txt"
let input = File.ReadAllLines(inputFile) |> Seq.toList;

type claim = { id: int; x : int; y : int; width : int; height : int }

let intersect c1 c2 =
        let checkX left right = right.x <= (left.x + left.width - 1)
        let checkY top bottom = bottom.y <= (top.y + top.height - 1)
        let intersecX = if c1.x < c2.x then checkX c1 c2 else checkX c2 c1
        let intersecY = if c1.y < c2.y then checkY c1 c2 else checkY c2 c1
        intersecX && intersecY

let parse (claimStr:string) =
    let s = claimStr.Split([|'#'; ' '; '@'; ','; ':'; 'x'|], StringSplitOptions.RemoveEmptyEntries) 
            |> Array.map int    
    { id = s.[0]; x = s.[1]; y = s.[2]; width = s.[3]; height = s.[4] }
        
let rec checkAll others claim =
    match others with
        | []                            -> true
        | h::t when claim = h           -> checkAll t claim
        | h::t when (intersect claim h) -> false
        | h::t                          -> checkAll t claim

let claims = input |> List.map parse

let r = claims |> List.filter (checkAll claims)