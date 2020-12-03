open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day08.txt")

type Counter = { Zero:int; One:int; Two:int }
    with static member Empty = { Zero = 0; One = 0; Two = 0}

let width = 25
let height = 6
let size = height * width

let line = File.ReadAllLines(file).[0]
let layers = [0 .. line.Length / size - 1] |> List.map (fun i -> line.Substring(i * size, size))

let folder state = function
    | '0' -> {state with Zero = state.Zero + 1 } 
    | '1' -> {state with One = state.One + 1 }
    | '2' -> {state with Two = state.Two + 1 }
    | x -> failwithf "Invalid case: %c" x

let result = layers 
            |> List.map (Seq.fold folder Counter.Empty) 
            |> List.sortBy (fun c -> c.Zero)
            |> List.map (fun c -> c.One * c.Two)
            |> List.head