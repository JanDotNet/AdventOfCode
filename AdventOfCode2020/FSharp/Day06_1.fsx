open System.IO
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day06.txt")

let foldGroups (agg, lst) = function
                            | "" -> ( [], agg :: lst)
                            | item -> ( (item |> Seq.toList) :: agg), lst

let allGroups = File.ReadAllLines(file) |> Seq.fold foldGroups ([], []) |> snd

let uniqueAnswersPerGroup group = 
    group |> (List.concat >> List.distinct >> List.length)

let result = allGroups |> List.map uniqueAnswersPerGroup |> List.sum