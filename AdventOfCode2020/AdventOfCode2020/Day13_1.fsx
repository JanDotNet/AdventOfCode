open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day13.txt")
let lines = File.ReadAllLines(file)
 
let timestamp = lines.[0] |> int
let busIds = lines.[1].Split(',') |> Array.toList |> List.filter (fun e -> e <> "x") |> List.map int

let nextTimestamp timestamp busId =
  let nextTimeStampFactor = ((timestamp |> float) / (busId |> float) |> int) + 1
  nextTimeStampFactor * busId
  
let nextBus = busIds 
              |> List.map (fun id -> (id, id |> nextTimestamp timestamp)) 
              |> List.minBy snd

let result = (nextBus |> fst) * ((nextBus |> snd) - timestamp) 