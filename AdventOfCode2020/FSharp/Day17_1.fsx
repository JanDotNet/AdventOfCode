open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day17.txt")
let input = File.ReadAllLines(file) |> Array.toList

(* Types *)
type Pos = { X:int; Y:int; Z:int }
type Loc = { Pos:Pos; State:char }

(* Parsing *)
let parseLines lines =
    let parseLine row line = 
        line |> Seq.mapi (fun col c -> { Pos = { X = col; Y = row; Z = 0}; State = c }) |> Seq.toList
    lines |> List.mapi parseLine |> List.collect id

let directions = seq {
  for x in [-1;0;1] do
    for y in [-1;0;1] do
      for z in [-1;0;1] do
        let res = (x, y, z)
        if res <> (0,0,0) 
        then yield res } |> Seq.toList

(* Change State *)
let nextState locs =
    let locsMap = locs |> List.map (fun x -> x.Pos, x.State) |> Map.ofList

    let getAdjacentPos (pos:Pos) =
      let translate (x, y, z) = { X = pos.X + x; Y = pos.Y + y; Z = pos.Z + z }
      directions |> List.map translate

    let getAdjacentStates (pos:Pos) =
        pos |> getAdjacentPos 
            |> List.map (fun p -> locsMap |> Map.tryFind p)
            |> List.map (fun state -> match state with 
                                      | Some(s) -> s 
                                      | None -> '.')            

    let expandLoc loc = seq {
        yield loc
        for neighborPos in loc.Pos |> getAdjacentPos do
          match locsMap.TryFind neighborPos with
          | Some(s) -> yield { Pos = neighborPos; State = s }
          | None    -> yield { Pos = neighborPos; State = '.' } } |> Seq.toList
    
    let countActiveNeighbors p = 
        p |> getAdjacentStates |> List.filter (fun s -> s = '#') |> List.length

    let swapLocState (locs, changed) loc = 
        match loc with
        | { Pos = pos; State = '#' } -> 
            let changeState = [2; 3] |> List.contains (pos |> countActiveNeighbors) |> not
            let loc' = { Pos = pos; State = if changeState then '.' else '#' }
            (loc' :: locs, changed || changeState)
        | { Pos = pos; State = '.' } -> 
            let changeState = (pos |> countActiveNeighbors) = 3
            let loc' = { Pos = pos; State = if changeState then '#' else '.' }
            (loc' :: locs, changed || changeState)
        | _ -> (loc :: locs, changed)

    let locsExpanded = locs |> List.map expandLoc |> List.collect id |> List.distinct
    
    locsExpanded |> List.fold swapLocState ([], false)

let rec run count locs =
  if count = 0 then locs
  else 
    printfn "run: %i " count
    (locs |> nextState |> fst) |> run (count - 1)

let result = input |> parseLines |> run 6 |> List.filter (fun s -> s.State = '#') |> List.length