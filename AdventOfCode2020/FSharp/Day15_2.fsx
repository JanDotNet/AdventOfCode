open System.Collections.Generic

let input = "8,13,1,0,18,9"
let initialNumbers = input.Split(',') |> Array.toList |> List.map (int >> int64) |> List.rev

let initialRow = int64(initialNumbers.Length)
let initialItem = initialNumbers.Head

let initialDict = new Dictionary<int64,int64>()
for (key, value) in initialNumbers.Tail |> List.rev |> List.mapi (fun idx item -> (item, int64(idx + 1))) do
  initialDict.Add(key, value)

let rec solve (dict:Dictionary<int64, int64>, lastItem) idx =
    let found, number = dict.TryGetValue lastItem
    let nextItem =  if found then idx - number else 0L
    dict.[lastItem] <- idx
    (dict, nextItem)

let result = [initialRow..(30000000L-1L)] |> List.fold solve (initialDict, initialItem)