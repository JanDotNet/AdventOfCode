open System
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day05.txt")

let codeToList s l (code:string) = code.Substring(s, l) |> Seq.rev |> Seq.toList
let splitRowAndColCodes code = code |> codeToList 0 7, code|> codeToList 7 3
let pow2 (x:int) = Math.Pow(2.0, x |> float) |> int

let rec decode bitChar (num, idx) = function
  | []      -> num
  | h :: t  -> let numNew =  num + (if h = bitChar then pow2 idx else 0)
               t |> (decode bitChar (numNew, idx+1))

let decodeRowCode = decode 'B' (0, 0)
let decodeColCode = decode 'R' (0, 0)

let result = File.ReadAllLines(file)
             |> Seq.map splitRowAndColCodes             
             |> Seq.map (fun (row, col) -> decodeRowCode row, decodeColCode col)
             |> Seq.map (fun (row, col) -> row * 8 + col)
             |> Seq.max