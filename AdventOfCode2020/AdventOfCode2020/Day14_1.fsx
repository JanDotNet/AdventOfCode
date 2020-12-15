open System
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day14.txt")
let lines = File.ReadAllLines(file) |> Array.toList
 
(* Types *)
type Program<'a> = { Mask:string; Mem:Map<int, 'a> }
  with static member Empty = { Mask = ""; Mem = Map.empty}

(* Helper *)
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

let run prog =
  { Mask = prog.Mask; Mem = prog.Mem |> Map.map (fun k v -> (v |> applyMask prog.Mask) |> binToDec bigint.Zero) }

(* parsing *)
let parseLines lines = 
  let fold (current, progs) (line:string) =
    if line.StartsWith("mask = ") then
      ( { Mask = line.Substring(7); Mem = Map.empty }, if current = Program<int>.Empty then progs else current :: progs )
    else
      let splitted = line.Split([|"mem["; "] = "|], StringSplitOptions.RemoveEmptyEntries)
      let pos = splitted.[0] |> int
      let value = (splitted.[1] |> int |> decToBin).PadLeft(36, '0')
      ( { current with Mem = current.Mem |> Map.add pos value },  progs)
  let parsed = lines |> List.fold fold (Program<int>.Empty, [])
  (fst parsed) :: (snd parsed)

(* final computation *)
let result = lines 
              |> parseLines 
              |> List.map run 
              |> List.map (fun p -> p.Mem |> Map.toList)
              |> List.collect id
              |> List.rev
              |> Map.ofList
              |> Map.toList
              |> List.map snd
              |> List.sum