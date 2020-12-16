open System
open System.IO
open System.Text.RegularExpressions

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day16.txt")
let lines = File.ReadAllLines(file) |> Array.toList

(* Helper *)
let rec transpose matrix = 
  match matrix with   // matrix is a list<list<int>>
  | row::rows ->      // case when the list of rows is non-empty
    match row with    // rows is a list<int>
    | col::cols ->    // case when the row is non-empty
      // Take first elements from all rows of the matrix
      let first = List.map List.head matrix
      // Take remaining elements from all rows of the matrix
      // and then transpose the resulting matrix
      let rest = transpose (List.map List.tail matrix) 
      first :: rest
    | _ -> []
  | _ -> [] 

(* Types *)
type Range = { Min:int64; Max: int64}
type Field = { Name:string; Rules:Range list }
type Ticket = int64 list    

(* Parsing *)
let parseField (line:string) : Field =
  let name = line.Split(':').[0]
  let ranges = seq {
    let matches = Regex.Matches(line, "(?<min>\d*)-(?<max>\d*)")
    for m in matches do
      yield { Min = m.Groups.["min"].Value |> int64; 
              Max = m.Groups.["max"].Value |> int64 } }
  { Name = name; Rules = ranges |> Seq.toList }

let parseTicket (line:string) : Ticket =
  line.Split(',') |> Array.toList |> List.map int64

let fieldLines = lines |> List.takeWhile (fun l -> l <> "")
let myTicketLine = lines |> List.skipWhile (fun l -> l <> "your ticket:") |> List.skip 1 |> List.take 1 |> List.head
let nearbyTicketLines = lines |> List.skipWhile (fun l -> l <> "nearby tickets:") |> List.skip 1

(* Validation *)
let matchRule number rule = number <= rule.Max && number >= rule.Min
let isValidNumber rules number = rules |> List.exists (matchRule number)
let isValidTicket rules ticket = ticket |> List.forall (isValidNumber rules)
let isValidField numbers field = numbers |> List.forall (isValidNumber field.Rules)

let fields = fieldLines |> List.map parseField
let allRules = fields |> List.collect (fun f -> f.Rules)
let myTicket = myTicketLine |> parseTicket
let allNearbyTickets = nearbyTicketLines |> List.map parseTicket
let validNearbyTickets = allNearbyTickets |> List.filter (isValidTicket allRules)
let numberByPosition = transpose validNearbyTickets |> List.mapi (fun idx pos -> (idx + 1, pos))

let rec resolveFields (posFields, fields, numByPos) =
  let getSingleMatchingField numbers =
    fields |> List.filter (isValidField numbers) |> List.tryExactlyOne

  if fields.IsEmpty then
      posFields
   else
      let posField = numByPos 
                      |> List.map (fun (pos, numbers) -> (pos, getSingleMatchingField numbers))
                      |> List.filter (fun (_, f) -> f |> Option.isSome)
                      |> List.map (fun (p, f) -> (p, f |> Option.get))
                      |> List.head
      resolveFields (posField :: posFields, fields |> List.except ([posField |> snd]), numByPos |> List.filter (fun (pos, _) -> pos <> (posField |> fst)))

let result = resolveFields ([], fields, numberByPosition) 
             |> List.filter (fun (_, field) -> field.Name.StartsWith("departure"))
             |> List.map (fun (pos, _) -> pos - 1)
             |> List.map (fun idx -> myTicket |> List.indexed |> List.find (fun (i, v) -> i = idx) |> snd)
             |> List.reduce (*)