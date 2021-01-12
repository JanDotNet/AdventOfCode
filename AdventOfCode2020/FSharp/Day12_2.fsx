open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day12.txt")
let input = File.ReadAllLines(file) |> Array.toList

(* Types *)
type State = { ShipX: int; ShipY: int; WaypointX: int; WaypointY: int } with 
  static member Initial = { ShipX = 0; ShipY = 0; WaypointX = 10; WaypointY = -1}

(* Parsing *)
let parseLine (line:string) = (line |> Seq.head, line.Substring(1) |> int)
  
(* State Change *)
let run instructions =
    let turnLeft s =   { s with WaypointX =  s.WaypointY; WaypointY = -s.WaypointX }
    let turnRight s =  { s with WaypointX = -s.WaypointY; WaypointY = s.WaypointX }
    let turnAround s = { s with WaypointX = -s.WaypointX; WaypointY = -s.WaypointY }   

    let rec processInstruction (s:State) = function
      | ('N', num) -> { s with WaypointY = s.WaypointY - num  }
      | ('E', num) -> { s with WaypointX = s.WaypointX + num  }
      | ('S', num) -> { s with WaypointY = s.WaypointY + num  }
      | ('W', num) -> { s with WaypointX = s.WaypointX - num  }
      | ('L',  90) 
      | ('R', 270) -> s |> turnLeft
      | ('R',  90) 
      | ('L', 270) -> s |> turnRight
      | ('L', 180) 
      | ('R', 180) -> s |> turnAround
      | ('F', num) -> { s with ShipX = s.ShipX + num * s.WaypointX; ShipY = s.ShipY + num * s.WaypointY }
      | (c, n)-> failwith (sprintf "Should not happen %c %i" c n)

    instructions |> List.fold processInstruction State.Initial

let getDistance (s:State) = abs (s.ShipY) + abs (s.ShipX)

let result = input |> List.map parseLine |> run |> getDistance
    