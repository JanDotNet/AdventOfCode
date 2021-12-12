open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day12.txt")

let input2 = "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"

//let input = input2.Split('\n') |> List.ofArray

//let input = File.ReadAllLines(file) |> List.ofArray

module Map =
    let append key value (map:Map<'a, 'b list>) = 
        if map.ContainsKey key
        then map |> Map.add key (value::map.[key])
        else map |> Map.add key [value]

let parse (lines:string list) =
    let collectionFolder (map:Map<string,string list>) (line:string) =
        let s = line.Split('-')        
        map |> Map.append s.[0] s.[1] |> Map.append s.[1] s.[0]
    lines |> List.fold collectionFolder Map.empty

let searchPathes (map:Map<string, string list>) =    
    let rec searchInner (map:Map<string, string list>)
                        (visitedSmallCaves:Set<string>)
                        (visitedBigCaves:Map<string,string list>)
                        (foundPathes:string list list)                        
                        (currentPath:string list) =     
        
        let isVisitableSmall nextCave = visitedSmallCaves |> Set.contains nextCave |> not
        let isVisitableBig currentCave nextCave =   match visitedBigCaves.TryFind currentCave with
                                                    | Some(nextCaves) -> nextCaves |> List.contains nextCave |> not
                                                    | None -> true

        let getNextVisitables currentCave nextCaves = 
            nextCaves |> List.filter (fun nextCave -> isVisitableSmall nextCave && isVisitableBig currentCave nextCave)
        
        let visitCave (currentCave:string) (nextCave:string) = if nextCave.[0] |> Char.IsUpper 
                                                               then visitedSmallCaves,  visitedBigCaves |> Map.append currentCave nextCave
                                                               else visitedSmallCaves |> Set.add nextCave, visitedBigCaves

        match currentPath with
        | [] -> searchInner map visitedSmallCaves visitedBigCaves foundPathes ["start"]
        | "end"::t -> searchInner map visitedSmallCaves visitedBigCaves (currentPath::foundPathes) t
        | currentCave::t 
            -> let nextVisitables = getNextVisitables currentCave map.[currentCave]
               if currentCave = "start" && t.Length > 0
                    then []
               else
                    let result = nextVisitables 
                                    |> List.map (fun nextCave -> let visitedSmallCaves', visitBigCaves' = visitCave currentCave nextCave
                                                                 searchInner map visitedSmallCaves' visitBigCaves' foundPathes (nextCave::currentPath))
                                    |> List.concat
                    result @ foundPathes
    searchInner map Set.empty Map.empty [] []

input |> parse |> searchPathes |> List.distinct |> List.map List.rev |> List.length