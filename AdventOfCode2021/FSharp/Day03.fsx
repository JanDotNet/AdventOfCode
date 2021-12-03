open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day03.txt")

//let input = ["00100";"11110";"10110";"10111";"10101";"01111";"00111";"11100";"10000";"11001";"00010";"01010"]
let input = File.ReadAllLines(file) |> Array.toList

let rec transpose = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

let getCommonBit equalValue bit (line:char list) =
    let folder agg x = if x = bit then agg + 1 else agg - 1    
    let result = line |> List.fold folder 0
    if result > 0 then 1 elif result < 0 then 0 else equalValue

let getMostCommonBit = (getCommonBit 1)
let getLeastCommonBit = (getCommonBit 0)

let entryToNumber list =
    list |> List.rev 
         |> List.indexed 
         |> List.fold (fun agg (i, num) -> if num = 1 then agg + int(2.0**float(i)) else agg) 0

let parse c = Int32.Parse(string(c))

let getRate bit diagnosticReport = 
    diagnosticReport 
        |> List.map (fun x -> x |> Seq.toList)
        |> transpose 
        |> List.map (getMostCommonBit bit) 
        |> entryToNumber

let getRating bitDeterminator diagnosticReport = 
    let dr = diagnosticReport |> List.map (fun x -> x |> Seq.toList)
    let rec inner posSkip remaining =
        match remaining with
        | [] -> failwith("remaining is empty")
        | [last] -> last |> List.map parse |> entryToNumber
        | rem -> let bit = rem |> transpose |> List.skip posSkip |> List.head |> (bitDeterminator)
                 let remaining' = remaining |> List.filter (fun remEntry -> remEntry |> List.skip posSkip |> List.head = char(string(bit)) )
                 inner (posSkip + 1) remaining'
    inner 0 dr

let solve1 diagnosticReport =
    let gammaRate = diagnosticReport |> getRate '1'
    let epsilonRate = diagnosticReport |> getRate '0'
    let powerConsumption = gammaRate * epsilonRate
    powerConsumption
    

let solve2 diagnosticReport =
    let co2ScrupbberRating = diagnosticReport |> getRating (getLeastCommonBit '0')
    let oxygenRating = diagnosticReport |> getRating (getMostCommonBit '1')
    let lifeSupportRating = co2ScrupbberRating * oxygenRating
    lifeSupportRating


printfn "Solution 1: %i" (solve1 input)
printfn "Solution 2: %i" (solve2 input)