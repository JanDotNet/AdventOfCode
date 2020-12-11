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
    let rec findNextState transform pos =
        let pos' = transform pos
        let state = locsMap |> Map.tryFind (pos')
        match state with
        | Some ('.') -> findNextState transform pos'
        | Some (x) -> Some(x)
        | None -> None
        
    let getAdjacentStates (pos:Pos) =
        seq {
          yield pos |> findNextState (fun p -> { p with X = X + 1})
          yield pos |> findNextState (fun p -> { p with X = x - 1})
          yield pos |> findNextState (fun p -> { p with Y = y + 1})
          yield pos |> findNextState (fun p -> { p with Y = y - 1})
          yield pos |> findNextState (fun p -> { X = X + 1; Y = Y + 1})
          yield pos |> findNextState (fun p -> { X = X + 1; Y = Y - 1})
          yield pos |> findNextState (fun p -> { X = X - 1; Y = Y + 1})
          yield pos |> findNextState (fun p -> { X = X - 1; Y = Y - 1})
        } |> Seq.filter Option.isSome |> Seq.map Option.get            

    let hasAnyOccupied p = 
        p |> getAdjacentStates |> Seq.contains '#'

    let hasFiverOrMoreOccupied p = 
        (p |> getAdjacentStates |> Seq.filter (fun x -> x = '#') |> Seq.length) >= 5

    let swapLocState (locs, changed) loc = 
        match loc with
        | { Pos = pos; State = '#' } -> 
            let changeState = pos |> hasFiverOrMoreOccupied
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
    