open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day12.txt")

let input2 = "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"

//let input = input2.Split('\n') |> List.ofArray

let input = File.ReadAllLines(file) |> List.ofArray

module Map =
    let append key value (map:Map<'a, 'b list>) = 
        if map.ContainsKey key
        then map |> Map.add key (value::map.[key])
        else map |> Map.add key [value]

    let except key value (map:Map<'a, 'b list>) = 
        if map.ContainsKey key
        then map |> Map.add key (map.[key] |> List.except [value])
        else map

let parse (lines:string list) =
    let collectionFolder (map:Map<string,string list>) (line:string) =
        let s = line.Split('-')        
        map |> Map.append s.[0] s.[1] |> Map.append s.[1] s.[0]
    lines |> List.fold collectionFolder Map.empty

let searchPathes maxSmallCaveVisit (map:Map<string, string list>) =    
    let rec searchInner (map:Map<string, string list>)
                        (visitedSmallCaves:list<string>)
                        (visitedBigCaves:Map<string,string list>)
                        (foundPathes:string list list)   
                        (currentPath:string list) =
        
        let isVisitableSmall nextCave = let isNotContained = visitedSmallCaves |> List.contains nextCave |> not
                                        let maxVisited = if visitedSmallCaves.Length > 0
                                                         then visitedSmallCaves |> List.groupBy id |> List.map (fun (k, l) -> l |> List.length) |> List.max
                                                         else 0
                                        isNotContained || maxVisited < maxSmallCaveVisit
        let isVisitableBig currentCave nextCave =   let currentExisting = visitedSmallCaves |> List.filter (fun x -> x = currentCave) |> List.length
                                                    match visitedBigCaves.TryFind currentCave with
                                                    | Some(nextCaves) -> nextCaves |> List.filter (fun x -> x = nextCave)
                                                                                   |> List.length < currentExisting
                                                    | None -> true

        let getNextVisitables currentCave nextCaves = 
            nextCaves |> List.filter (fun nextCave -> isVisitableSmall nextCave && isVisitableBig currentCave nextCave)
        
        let visitCave (currentCave:string) (nextCave:string) = let hasVisited = visitedSmallCaves |> List.contains nextCave
                                                               if nextCave.[0] |> Char.IsUpper 
                                                               then visitedSmallCaves,  visitedBigCaves |> Map.append currentCave nextCave
                                                               else nextCave :: visitedSmallCaves , if hasVisited 
                                                                                                    then visitedBigCaves |> Map.except currentCave nextCave
                                                                                                    else visitedBigCaves

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
    searchInner map [] Map.empty [] []

let solve1 input = input |> parse |> searchPathes 1 |> List.distinct |> List.map List.rev |> List.sort |> List.length
let solve2 input = input |> parse |> searchPathes 2 |> List.distinct |> List.map List.rev |> List.sort |> List.length

printfn "Solution 1: %i" (input |> solve1)
printfn "Solution 2: %i" (input |> solve2)