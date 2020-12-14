open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day13.txt")

// WIP
//let line = File.ReadAllLines(file)
//              |> Array.skip 1
//              |> Array.exactlyOne

let line = "7,13,x,x,59,x,31,19"
let split (l:string) = l.Split(',')
                    |> Array.toList
                    |> List.mapi (fun idx el -> (idx, el))
                    |> List.filter (fun (_, el) -> el <> "x") 
                    |> List.map (fun (idx, el) -> (el |> int |> bigint ), idx |> bigint )
                    |> List.sortBy snd
                    |> List.rev

let busIds = split line
 
let busId_0 = busIds |> List.head |> snd 
let busIds_rem = busIds.Tail

let rec solve timestamp =
  //printfn "%s" timestamp.ToString()
  let found = busIds_rem |> List.forall (fun (num, id) -> (bigint.Add (timestamp, num)) % (id) = bigint.Zero)
  if found then timestamp
  else solve (timestamp + busId_0)

solve busId_0