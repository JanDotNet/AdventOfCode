open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day01.txt")

let calcFuel x = x / 3 - 2
let rec calcFuelRec sum remaining =
    let result = remaining |> calcFuel
    if result <= 0 
    then sum
    else calcFuelRec (sum + result) result

File.ReadAllLines(file) 
    |> Array.map (int >> calcFuelRec 0)
    |> Array.sum

