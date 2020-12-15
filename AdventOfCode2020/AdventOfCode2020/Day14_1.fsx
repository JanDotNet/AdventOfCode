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
  | _ ->
      let bit = string (dec % 2)
      (decToBin (dec / 2)) + bit

let rec binToDec dec bin =
  match bin with
  | [] -> dec
  | '0'::t -> binToDec dec t
  | '1'::t -> binToDec (bigint.Add(dec, (bigint.Pow(2I, t.Length)))) t
  | x::t -> failwithf "Invalid char in binary string: %c" x

let applyMask mask value =
  (value |> Seq.zip mask) |> Seq.map (fun (m, v) -> if m <> 'X' then m else v) |> Seq.toList

let run (mask, key, value) =
  (key, (value |> applyMask mask) |> binToDec bigint.Zero)

(* parsing *)
let parseLines lines = 
  let fold (mask, progs) (line:string) =
    if line.StartsWith("mask = ") then
      ( line.Substring(7), progs )
    else
      let splitted = line.Split([|"mem["; "] = "|], StringSplitOptions.RemoveEmptyEntries)
      let pos = splitted.[0] |> int
      let value = (splitted.[1] |> int |> decToBin |> padLeft 36 '0')
      ( mask, (mask, pos, value) :: progs)
  lines |> List.fold fold ("", []) |> snd

(* final computation *)
let result = lines 
              |> parseLines 
              |> List.map run
              |> List.rev
              |> Map.ofList
              |> Map.toList
              |> List.map snd
              |> List.sum