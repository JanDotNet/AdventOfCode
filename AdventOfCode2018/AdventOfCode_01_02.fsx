#r "../packages/FSharp.Data/lib/netstandard2.0/FSharp.Data.dll"

open System
open System.IO
open FSharp.Data
open System.Collections.Generic

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_01.txt"
let input2 = File.ReadAllLines(inputFile);

let input = [1;-2;3;1]

type State =
    | FinalState of int
    | IntermediateState of int * int Set

type State with
    static member Zero = IntermediateState(0, Set.empty.Add(0)) 

let foldIntermediateState (freqHistory: int Set, freq : int) : State =
    let isFinal = freqHistory |> Set.contains freq
    if isFinal then
        FinalState (freq)
    else
        IntermediateState (freq, freqHistory.Add(freq))

let foldState state value =
    match state with
        | FinalState _ -> state
        | IntermediateState (lastFreq, freqHistory) -> foldIntermediateState (freqHistory, lastFreq + value)
       
let rec processListRec state lst =
    let result = lst |> Seq.fold foldState state
    match result with
        | FinalState result -> printfn "The result is: %i" result
        | IntermediateState _ -> lst |> processListRec result

input |> Seq.map int |> processListRec State.Zero