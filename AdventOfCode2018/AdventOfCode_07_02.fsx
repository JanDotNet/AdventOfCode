open System
open System.IO

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_07.txt"
let input = File.ReadAllLines(inputFile) |> Seq.toList;

(* Config *)
let workerCount = 5
let getStepDuration s =
    let baseVal = 60
    let offset = - ('A' |> int) + 1
    let value = s |> int
    baseVal + value + offset

let test = ['A'; 'B'; 'C'] |> List.map getStepDuration
    
let a = 'C' |> getStepDuration

(* Types *)
type StepInfo = 
    { Step:char
      Duration:int
      PrevSteps:char list
      NextSteps:char list }

type StepInProgress =
    { Info:StepInfo 
      Remaining:int }

(* Helper Functions *)
let toString charList = charList |> Array.ofList |> String
let parse line = 
    line 
    |> Seq.toArray 
    |> (fun s -> (s.[5], s.[36]))

let groupByTuple keySelector valueSelector list =
    list 
    |> List.groupBy keySelector 
    |> List.map (fun (k, g) -> (k, g |> List.map valueSelector |> List.sort))

let getGroup key list =
    list  
    |> List.tryPick (fun (k, g) -> if k = key then Some (g) else None) 
    |> Option.defaultValue []; 

(* Main Logic *) 
let rawInput = input |> List.map parse
let next = rawInput |> groupByTuple fst snd
let prev = rawInput |> groupByTuple snd fst

let allSteps = ((next |> List.map fst) @ (prev |> List.map fst)) |> List.distinct
let toStepInfo s = { Step = s; Duration = s |> getStepDuration; PrevSteps = prev |> getGroup s; NextSteps = next |> getGroup s; }
let toStepInProgress si = { Info = si; Remaining = si.Duration }
let stepInfos = allSteps |> List.map toStepInfo
let decrementDuration si = { si with Remaining = si.Remaining - 1 }
let findStepInfo s = stepInfos |> List.find (fun i -> i.Step = s)

let getResult allSteps =
    let rec loop state allStepsToProgress =
        let processed, time = state
        (* helper *)
        let arePrevStatesProcessed si = si.Info.PrevSteps |> List.forall (fun x -> processed |> Seq.contains x)
        let sortDistinct lst = lst |> List.distinctBy (fun x -> x.Info.Step) |> List.sortBy (fun x -> x.Info.Step) 
       
        (* logic *)
        let workingSteps = 
            allStepsToProgress 
            |> List.filter arePrevStatesProcessed 
            |> List.truncate workerCount

        let remainingSteps =
            allStepsToProgress
            |> List.except workingSteps
        
        let finishedSteps = workingSteps |> List.map decrementDuration |> List.filter (fun si -> si.Remaining = 0)
        let unfinishedSteps = workingSteps |> List.map decrementDuration |> List.filter (fun si -> si.Remaining > 0)
        let newProcessed = processed + (finishedSteps |> List.map (fun sip -> sip.Info.Step) |> toString)
        let newState = (newProcessed, time + 1)

        let nextStepsToProgress = 
            finishedSteps 
            |> List.collect (fun s -> s.Info.NextSteps) 
            |> List.filter (fun s -> newProcessed |> Seq.contains s |> not)
            |> List.filter (fun s -> allStepsToProgress |> List.map (fun stp -> stp.Info.Step) |> List.contains s |> not)
            |> List.map (toStepInfo >> toStepInProgress)
        let stepsToProgress = unfinishedSteps @ (remainingSteps @ nextStepsToProgress |> List.sortBy (fun x -> x.Info.Step)) 

        match stepsToProgress with
        | [] -> newState
        | _ -> loop newState stepsToProgress

    let firstSteps = allSteps |> List.filter (fun x -> x.PrevSteps.Length = 0) |> List.map toStepInProgress
        
    loop ("", 0) firstSteps

getResult stepInfos