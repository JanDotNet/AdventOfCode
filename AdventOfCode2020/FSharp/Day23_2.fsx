let size = 1_000_000
let iterations = 10_000_000
let connections = [|0..size|]
let initialBeginning = "624397158" |> List.ofSeq |> List.map (string >> int)
let initial = initialBeginning @ [10..size]

for (l, r) in (initial |> List.pairwise) do
  connections.[l] <- r
connections.[initial |> List.last] <- initial.Head

let getSplice cur =
  let mutable threeItems = [connections.[cur]]
  for _ in [1..2] do
    threeItems <- threeItems @ [connections.[threeItems |> List.last]]
  threeItems

let getDestination cur three =
  let next c = if c = 1 then size else c-1
  let mutable cur' = next cur
  while three |> List.contains cur' do
    cur' <- next cur'
  cur'

let swap splice dest cur =
  let lastInSplice = splice |> List.last 
  connections.[cur] <- connections.[lastInSplice]
  connections.[lastInSplice] <- connections.[dest]
  connections.[dest] <- splice |> List.head

let mutable splice = [1;2;3]
let mutable dest = 0
let mutable current = 6
for i = 1 to iterations do
  splice <- getSplice current
  dest <- getDestination current splice
  swap splice dest current
  current <- connections.[current]

let second = connections.[1] |> bigint     
let third = connections.[connections.[1]] |> bigint
let result = second * third