open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day12.txt")
let input = File.ReadAllLines(file) |> Array.toList

(* Types *)
type State = { Dir:char; ShipX: int; ShipY: int } with 
  static member Initial = { Dir = 'E'; ShipX = 0; ShipY = 0}

(* Parsing *)
let parseLine (line:string) = (line |> Seq.head, line.Substring(1) |> int)
  
(* State Change *)
let run instructions =
    let turnLeft = [('N', 'W'); ('W', 'S'); ('S', 'E'); ('E', 'N')]
    let turnRight = [('N', 'E'); ('E', 'S'); ('S', 'W'); ('W', 'N')]
    let turnAround = [('N', 'S'); ('W', 'E'); ('E', 'W'); ('S', 'N')]    
    let apply turns d = 
      turns |> List.find (fun (s, _) -> s = d) |> snd

    let rec processInstruction state = function
      | ('N', num) -> { state with ShipY = state.ShipY - num }
      | ('E', num) -> { state with ShipX = state.ShipX + num  }
      | ('S', num) -> { state with ShipY = state.ShipY + num }
      | ('W', num) -> { state with ShipX = state.ShipX - num }
      | ('L',  90) 
      | ('R', 270) -> { state with Dir = state.Dir |> apply turnLeft }
      | ('R',  90) 
      | ('L', 270) -> { state with Dir = state.Dir |> apply turnRight }
      | ('L', 180) 
      | ('R', 180) -> { state with Dir = state.Dir |> apply turnAround }
      | ('F', num) -> processInstruction state (state.Dir, num)
      | (c, n)-> failwith (sprintf "Should not happen %c %i" c n)

    instructions |> List.fold processInstruction State.Initial

let getDistance (s:State) = abs (s.ShipX) + abs (s.ShipY)

let result = input |> List.map parseLine |> run |> getDistance
    