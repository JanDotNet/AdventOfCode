open System
open System.IO
open System.Globalization

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_04.txt"
let input = File.ReadAllLines(inputFile) |> Seq.toList;

type Event =
    | WakeUp of DateTime
    | FallsAsleep of DateTime
    | BeginsShift of DateTime * int

type GuardRelatedItem =
    | ID of int
    | SleepMinutes of int list

type GuardAggregate = { Id: int; SleepMinutes : int list}

let toResult (g:GuardAggregate) =
    let sum = g.SleepMinutes.Length
    let maxFrequency = [0..59]
                       |> List.map (fun x -> (x, g.SleepMinutes |> List.filter (fun y -> y = x) |> List.length))
                       |> List.sortByDescending (fun (_, c) -> c)
                       |> List.head
    (g.Id, sum, fst maxFrequency, snd maxFrequency)

let parseTimestamp (line:string) =
        let timeStampStr = line.Substring(1, 16);
        DateTime.ParseExact(timeStampStr, "yyyy-MM-dd HH:mm", CultureInfo.InvariantCulture)

let parseEvent (line:string) =    
    let parseId (line:string) =
        line.Split(' ') 
            |> Seq.find (fun x -> x.StartsWith("#"))
            |> Seq.skip 1 
            |> String.Concat|> int

    let ts = (line |> parseTimestamp)
    if (line.EndsWith("wakes up")) then WakeUp ts
    else if line.EndsWith("falls asleep") then FallsAsleep ts
    else if (line.EndsWith("begins shift")) then BeginsShift (ts, (line |> parseId))
    else failwith("Invalid case: " + line)

let getEventTimestamp = function
    | WakeUp d -> d
    | FallsAsleep d -> d
    | BeginsShift (d, _) -> d

let getAllMinutes leftDate rightDate = 
    let rec loop (d1:DateTime) (d2:DateTime) =
        seq {
            if (d1 < d2) then 
                yield d1.Minute
                yield! loop (d1.AddMinutes 1.0) d2 }
    loop leftDate rightDate |> Seq.toList

let combineToGuardRelatedItem = function
        | (BeginsShift (d1, id), FallsAsleep d2) -> Some(ID(id))
        | (FallsAsleep d1, WakeUp d2) -> Some(SleepMinutes(getAllMinutes d1 d2) )
        | (WakeUp _, BeginsShift _) -> None
        | (WakeUp _, FallsAsleep _) -> None
        | (BeginsShift (_, id1), BeginsShift (_, id2)) -> Some(ID(id1))
        | (x, y) -> failwith ("should not happen" + x.ToString() + ";" + y.ToString())

let combineToGuardAggregate (s: GuardAggregate list) (item : GuardRelatedItem) : GuardAggregate list =
    match item with
        | ID (id) -> {Id = id; SleepMinutes = []} :: s
        | SleepMinutes (minutes) -> { s.Head with SleepMinutes = minutes @ s.Head.SleepMinutes } :: s.Tail

let aggregateSortedGuardAggregate state (item:GuardAggregate) =
    match state with
        | h::t when h.Id = item.Id -> { h with SleepMinutes = h.SleepMinutes @ item.SleepMinutes } :: t
        | h::t when h.Id <> item.Id -> item :: state
        | [] -> [item]
        | _ -> failwith "should not happen"

let rawResult = input
                |> List.map parseEvent
                |> List.sortBy (fun e -> e |> getEventTimestamp)
                |> List.pairwise
                |> List.map combineToGuardRelatedItem
                |> List.filter Option.isSome
                |> List.map Option.get
                |> List.fold combineToGuardAggregate []
                |> List.sortBy (fun x -> x.Id)
                |> List.fold aggregateSortedGuardAggregate []
                |> List.map toResult
                |> List.sortByDescending (fun (id, total, maxfreq, count) -> count)
                |> List.map (fun (a, b, c, d) -> (a, b, c, d, a*c))
                
rawResult |> List.map (fun x -> x.ToString()) |> List.iter (fun x -> printfn "%s" x)