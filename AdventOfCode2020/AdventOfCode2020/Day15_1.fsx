let input = "8,13,1,0,18,9"
let initialNumbers = input.Split(',') |> Array.toList |> List.map int |> List.rev

let initialRow = initialNumbers.Length
let initialItem = initialNumbers.Head
let initialItems = initialNumbers.Tail |> List.rev
let initialMap = initialItems |> List.mapi (fun idx item -> (item, idx + 1)) |> Map.ofList

let rec solve (map, lastItem) idx =
  let nextItem =  match map |> Map.tryFind lastItem with
                    | Some (number) ->  idx - number
                    | None -> 0
  let nextMap = map |> Map.add lastItem idx
  (nextMap, nextItem)

let result = [initialRow..(2020-1)] |> List.fold solve (initialMap, initialItem)