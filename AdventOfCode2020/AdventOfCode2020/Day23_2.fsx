let getDestination curIdx (vbi: int array) (ibv:int array) =
  let curValue = vbi.[curIdx]
  let lastIdx = vbi.Length - 1
  let isValueInRange v = 
    let idx = ibv.[v]
    if curIdx = lastIdx then idx <> 0 && idx <> 1 && idx <> 2
    elif curIdx = (lastIdx - 1) then idx <> 0 && idx <> 2 && idx <> lastIdx
    elif curIdx = (lastIdx - 2) then idx <> 0 && idx <> lastIdx && idx <> (lastIdx - 1)
    else (idx < curIdx || idx > (curIdx + 3))
  let value = seq { for v = (curValue-1) downto 1 do yield v } 
              |> Seq.tryFind isValueInRange
  match value with
  | Some (x) -> x
  | None -> seq { for v = vbi.Length downto 1 do yield v } 
            |> Seq.find isValueInRange

let swap curIdx destValue (vbi: int array) (ibv:int array) (swapItems:int array) =
  let destIdx = ibv.[destValue]
  let lastIdx = vbi.Length-1
  let inline toValidIdx idx = 
    if idx > lastIdx then idx - lastIdx - 1 else idx

  // CASE 1
  if curIdx = (lastIdx - 2) then
    swapItems.[0] <- vbi.[lastIdx-1]
    swapItems.[1] <- vbi.[lastIdx]
    swapItems.[2] <- vbi.[0]
    for idx = 1 to destIdx do
      let value = vbi.[idx]
      vbi.[idx-1] <- value
      ibv.[value] <- idx-1
    for idx = curIdx downto (destIdx+1) do
      let value = vbi.[idx]
      vbi.[idx+2] <- value
      ibv.[value] <- idx+2
    vbi.[destIdx]   <- swapItems.[0]
    vbi.[destIdx+1] <- swapItems.[1]
    vbi.[destIdx+2] <- swapItems.[2]
    ibv.[swapItems.[0]] <- destIdx
    ibv.[swapItems.[1]] <- destIdx+1
    ibv.[swapItems.[2]] <- destIdx+2
    0
  // CASE 2
  elif curIdx = (lastIdx - 1) then
    swapItems.[0] <- vbi.[lastIdx]
    swapItems.[1] <- vbi.[0]
    swapItems.[2] <- vbi.[1]
    for idx = 2 to destIdx do
      let value = vbi.[idx]
      vbi.[idx-2] <- value
      ibv.[value] <- idx-2
    for idx = curIdx downto (destIdx+1) do
      let value = vbi.[idx]
      vbi.[idx+1] <- value
      ibv.[value] <- idx+1
    vbi.[destIdx-1] <- swapItems.[0]
    vbi.[destIdx]   <- swapItems.[1]
    vbi.[destIdx+1] <- swapItems.[2]
    ibv.[swapItems.[0]] <- destIdx-1
    ibv.[swapItems.[1]] <- destIdx
    ibv.[swapItems.[2]] <- destIdx+1
    0
  // CASE 3
  elif curIdx = lastIdx then
    swapItems.[0] <- vbi.[0]
    swapItems.[1] <- vbi.[1]
    swapItems.[2] <- vbi.[2]
    for idx = 3 to destIdx do
      let value = vbi.[idx]
      vbi.[idx-3] <- value
      ibv.[value] <- idx-3    
    vbi.[destIdx-2] <- swapItems.[0]
    vbi.[destIdx-1] <- swapItems.[1]
    vbi.[destIdx]   <- swapItems.[2]
    ibv.[swapItems.[0]] <- destIdx-2
    ibv.[swapItems.[1]] <- destIdx-1
    ibv.[swapItems.[2]] <- destIdx
    0
  // CASE 4
  elif destIdx < curIdx then
    swapItems.[0] <- vbi.[curIdx+1]
    swapItems.[1] <- vbi.[curIdx+2]
    swapItems.[2] <- vbi.[curIdx+3]
    for idx = curIdx downto (destIdx+1) do
      let value = vbi.[idx]
      vbi.[idx+3] <- value
      ibv.[value] <- idx+3
    vbi.[destIdx+1] <- swapItems.[0]
    vbi.[destIdx+2] <- swapItems.[1]
    vbi.[destIdx+3]   <- swapItems.[2]
    ibv.[swapItems.[0]] <- destIdx+1
    ibv.[swapItems.[1]] <- destIdx+2
    ibv.[swapItems.[2]] <- destIdx+3
    curIdx + 4 |> toValidIdx
  // CASE 5
  elif destIdx > curIdx then
    swapItems.[0] <- vbi.[curIdx+1]
    swapItems.[1] <- vbi.[curIdx+2]
    swapItems.[2] <- vbi.[curIdx+3]
    for idx = (curIdx+4) to destIdx do
      let value = vbi.[idx]
      vbi.[idx-3] <- value
      ibv.[value] <- idx-3    
    vbi.[destIdx-2] <- swapItems.[0]
    vbi.[destIdx-1] <- swapItems.[1]
    vbi.[destIdx]   <- swapItems.[2]
    ibv.[swapItems.[0]] <- destIdx-2
    ibv.[swapItems.[1]] <- destIdx-1
    ibv.[swapItems.[2]] <- destIdx
    curIdx + 1 |> toValidIdx
  else 
    failwith "Should not happen"

let size = 1000000
let iterations = 10000000
let valuesByIndex = [|1..size|]
let indicesByValues = [|0..size|]
let initial = "624397158" |> Array.ofSeq |> Array.map (string >> int)

for (idx, value) in (initial |> Array.indexed) do
  valuesByIndex.[idx] <- value
  indicesByValues.[value] <- idx

let mutable destination = 0
let mutable currentIndex = 0
let swapItems = [|1..3|]
for round=1 to iterations do
  if round % 1000 = 0 then printfn "Round: %i" round else () 
  destination <- getDestination currentIndex valuesByIndex indicesByValues
  currentIndex <- swap currentIndex destination valuesByIndex indicesByValues swapItems

let result = valuesByIndex 
             |> Array.skipWhile ((<>)1) 
             |> Array.skip 1 
             |> Array.take 2 
             |> Array.map bigint 
             |> Array.reduce (*)