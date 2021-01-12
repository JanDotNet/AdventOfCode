open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day13.txt")

let line = File.ReadAllLines(file)
             |> Array.skip 1
             |> Array.exactlyOne

let parse (l:string) = l.Split(',')
                    |> Array.toList
                    |> List.mapi (fun idx el -> (idx, el))
                    |> List.filter (fun (_, el) -> el <> "x") 
                    |> List.map (fun (idx, el) -> (el |> int |> bigint ),  ((el |> int)-idx) |> bigint )
                    |> List.sortBy snd

let calcXi ni Ni =
  Seq.initInfinite (fun i -> bigint(i + 1))
  |> Seq.map (fun x -> Ni * x % ni, x)
  |> Seq.find (fun (rem, x) -> rem = bigint(1))
  |> snd

let busIds = parse line
let N = busIds |> List.map fst |> List.reduce (*)
let result = busIds 
             |> List.map (fun (ni, bi) -> (ni, bi, N / ni))
             |> List.map (fun (ni, bi, Ni) -> (bi, Ni, calcXi ni Ni))
             |> List.map (fun (bi, Ni, xi) -> bi * Ni * xi)
             |> List.sum
              
let result' = result % N