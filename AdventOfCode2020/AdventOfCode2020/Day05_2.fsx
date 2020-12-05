open System
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day05.txt")

let getSubCode idx len (code:string) = code.Substring(idx, len) |> Seq.rev |> Seq.toList
let splitRowAndColCodes code = code |> getSubCode 0 7, code|> getSubCode 7 3

let rec decode bitChar (num, idx) = function
  | []      -> num
  | h :: t  -> let numNew =  num + (if h = bitChar then pown 2 idx else 0)
               t |> (decode bitChar (numNew, idx+1))

let decodeRowCode = decode 'B' (0, 0)
let decodeColCode = decode 'R' (0, 0)
let decodeRowAndColCode (rowCode, colCode) = decodeRowCode rowCode, decodeColCode colCode
let codeToSeatId (row, col) = row * 8 + col
let areNumbersNotAdjacent(n1:int, n2:int) = abs (n1-n2) <> 1
let getNumberBeween (n1, n2) = (n1 + n2) / 2

let result = File.ReadAllLines(file)
             |> Seq.map splitRowAndColCodes             
             |> Seq.map decodeRowAndColCode
             |> Seq.map codeToSeatId
             |> Seq.sort
             |> Seq.pairwise
             |> Seq.filter areNumbersNotAdjacent
             |> Seq.map getNumberBeween
             |> Seq.exactlyOne