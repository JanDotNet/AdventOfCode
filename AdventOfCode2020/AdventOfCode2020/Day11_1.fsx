open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day11.txt")
let input = File.ReadAllLines(file) |> Array.toList

(* Types *)
type Pos = { X:int; Y:int }
type Loc = { Pos:Pos; State:char }

(* Parsing *)
let parseLines lines =
    let parseLine row line = 
        line |> Seq.mapi (fun col c -> { Pos = { X = col; Y = row}; State = c }) |> Seq.toList
    lines |> List.mapi parseLine |> List.collect id

(* Change State *)
let nextState locs =
    let locsMap = locs |> List.map (fun x -> x.Pos, x.State) |> Map.ofList

    let getAdjacentPos (pos:Pos) =
        [ { pos with X = pos.X - 1};
          { pos with X = pos.X + 1};
          { pos with Y = pos.Y - 1};
          { pos with Y = pos.Y + 1};
          { X = pos.X - 1; Y = pos.Y - 1};
          { X = pos.X - 1; Y = pos.Y + 1};
          { X = pos.X + 1; Y = pos.Y - 1};
          { X = pos.X + 1; Y = pos.Y + 1} ]

    let getAdjacentStates (pos:Pos) =
        pos |> getAdjacentPos 
            |> List.map (fun p -> locsMap |> Map.tryFind p)
            |> List.filter Option.isSome
            |> List.map Option.get
            |> List.map (fun state -> state)
            

    let hasAnyOccupied p = 
        p |> getAdjacentStates |> List.contains '#'

    let hasfourOrMoreOccupied p = 
        (p |> getAdjacentStates |> List.filter (fun x -> x = '#') |> List.length) >= 4

    let swapLocState (locs, changed) loc = 
        match loc with
        | { Pos = pos; State = '#' } -> 
            let changeState = pos |> hasfourOrMoreOccupied
            let loc' = { Pos = pos; State = if changeState then 'L' else '#' }
            (loc' :: locs, changed || changeState)
        | { Pos = pos; State = 'L' } -> 
            let changeState = pos |> (hasAnyOccupied >> not)
            let loc' = { Pos = pos; State = if changeState then '#' else 'L' }
            (loc' :: locs, changed || changeState)
        | _ -> (loc :: locs, changed)

    locs |> List.fold swapLocState ([], false)

let run locs =
    let rec run' (locs', hasChanged) =
        if not hasChanged then locs' else run' (locs' |> nextState)
    run' (locs, true)

let result = input |> parseLines |> run |> List.filter (fun l -> l.State = '#') |> List.length
    