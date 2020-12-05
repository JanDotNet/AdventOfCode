open System
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day05.txt")

let toList s l (code:string) = code.Substring(s, l) |> Seq.rev |> Seq.toList
let splitCodes code = code |> toList 0 7, code|> toList 7 3
let pow2 (x:int) = Math.Pow(2.0, x |> float) |> int

let rec decode oneChar zeroChar (num, idx) = function
  | []  -> num
  | h :: t -> let numNew =  num + (if h = zeroChar then 0 else pow2 idx)
              t |> (decode oneChar zeroChar (numNew, idx+1))

let decodeRowCode = decode 'B' 'F' (0, 0)
let decodeColCode = decode 'R' 'L' (0, 0)

let result = File.ReadAllLines(file)
             |> Seq.map splitCodes             
             |> Seq.map (fun (row, col) -> decodeRowCode row, decodeColCode col)            
             |> Seq.map (fun (row, col) -> row * 8 + col)
             |> Seq.sort
             |> Seq.pairwise
             |> Seq.filter (fun (prev, cur) -> prev <> (cur - 1) && cur <> (prev - 1))
             |> Seq.map (fun (prev, cur) -> (prev + cur) / 2)
             |> Seq.exactlyOne