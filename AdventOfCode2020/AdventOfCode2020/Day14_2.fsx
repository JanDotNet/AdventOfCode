open System
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day14.txt")
let lines = File.ReadAllLines(file) |> Array.toList
 
(* Helper *)
let padLeft n c (s:string) =
  s.PadLeft(n, c)

let rec decToBin dec =
  match dec with
  | 0 | 1 -> string dec
  | _ -> (decToBin (dec / 2)) + string (dec % 2)      

let rec binToDec dec bin =
  match bin with
  | [] -> dec
  | '0'::t -> binToDec dec t
  | '1'::t -> binToDec (bigint.Add(dec, (bigint.Pow(2I, t.Length)))) t
  | x::t -> failwithf "Invalid char in binary string: %c" x

let generatFloatings n 
    = [0..(pown 2 n)-1] |> List.map (fun x -> x |> decToBin |> padLeft n '0' |> Seq.map id |> Seq.toList)

let applyMask mask value =
    let masked = (value |> Seq.zip mask) |> Seq.map (fun (m, v) -> if m <> '0' then m else v) |> Seq.toList
    let numOfFloatingBits = masked |> Seq.filter (fun x -> x = 'X') |> Seq.length
    let floatingVariations = generatFloatings numOfFloatingBits |> Seq.toList
    let folder (res, f:char list) i = if i = 'X' then (f.Head :: res, f.Tail) else (i :: res, f)
    floatingVariations |> List.map (fun float -> masked |> List.fold folder ([], float) |> fst |> List.rev)

let run (mask, pos, (value:int)) =
  let binPositions = pos |> decToBin |> padLeft 36 '0' |> applyMask mask
  binPositions |> List.map (fun bPos -> (bPos |> binToDec 0I, bigint(value)))

(* parsing *)
let parseLines lines = 
  let fold (mask, progs) (line:string) =
    if line.StartsWith("mask = ") then
      ( line.Substring(7), progs )
    else
      let splitted = line.Split([|"mem["; "] = "|], StringSplitOptions.RemoveEmptyEntries)
      let pos = splitted.[0] |> int
      let value = (splitted.[1] |> int)
      ( mask, (mask, pos, value) :: progs)
  lines |> List.fold fold ("", []) |> snd

(* final computation *)
let result = lines 
              |> parseLines 
              |> List.map run 
              |> List.collect id
              |> List.distinctBy fst
              |> List.sumBy snd