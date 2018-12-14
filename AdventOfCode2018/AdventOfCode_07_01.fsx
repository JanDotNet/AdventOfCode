open System
open System.IO

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_07.txt"
let input = File.ReadAllLines(inputFile) |> Seq.toList;

let parse line = 
    let s = line |> Seq.toArray
    (s.[5], s.[36])

type StepInfo = { Step:Char; PrevSteps:char list; NextSteps:char list }
 
let rawInput = input |> List.map parse
let next = rawInput |> List.groupBy fst |> List.map (fun (k, g) -> (k, g |> List.map snd |> List.sort))
let prev = rawInput |> List.groupBy snd |> List.map (fun (k, g) -> (k, g |> List.map fst |> List.sort))
let steps = ((next |> List.map fst) @ (prev |> List.map fst)) |> List.distinct
let stepInfos = 
    steps |> List.map (fun s -> 
                            { Step = s;
                              PrevSteps = prev |> List.tryPick (fun (k, g) -> if k = s then Some (g) else None) |> Option.defaultValue [];
                              NextSteps = next |> List.tryPick (fun (k, g) -> if k = s then Some (g) else None) |> Option.defaultValue [] })

let toStepInfo c = stepInfos |> List.find (fun i -> i.Step = c)

let getResult steps =
    let rec loop state stack =
        let prevStatesValid si = si.PrevSteps |> List.forall (fun x -> state |> Seq.contains x)
        let nextElement = stack |> List.filter prevStatesValid |> List.sort |> List.tryHead
        let sortDistinct lst = lst |> List.distinctBy (fun x -> x.Step) |> List.sortBy (fun x -> x.Step) 
        match nextElement with
        | Some (ne) -> loop (state + ne.Step.ToString()) (((ne.NextSteps |> List.map toStepInfo) @ (stack |> List.except [ne])) |> sortDistinct)
        | None -> state

    let first = steps |> List.filter (fun x -> x.PrevSteps.Length = 0)
        
    loop "" first

getResult stepInfos