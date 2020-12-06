open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day06.txt")

let foldGroups (agg, lst) = function
                            | "" -> ( [], agg :: lst)
                            | item -> ( (item |> Seq.toList) :: agg), lst

let allGroups = File.ReadAllLines(file) |> Seq.fold foldGroups ([], []) |> snd
let uniqueQuestions = allGroups |> (List.concat >> List.concat >> List.distinct)

let hasPersonAnsweredQuestion question person = 
    person |> List.contains question

let haveAllPersonsAnswerdQuestion group question = 
    group |> List.forall (hasPersonAnsweredQuestion question)

let countQuestionsAnsweredFromAll group = 
    uniqueQuestions |> List.filter (haveAllPersonsAnswerdQuestion group) |> List.length

let result = allGroups |> List.map countQuestionsAnsweredFromAll |> List.sum