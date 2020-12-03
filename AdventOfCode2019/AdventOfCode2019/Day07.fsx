open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day07.txt")

let input = File.ReadAllText(file).Split(',') |> Array.map int

(*Helper*)
module Seq = 
    let rec cycle xs = seq { yield! xs; yield! cycle xs }

(* Domain *)
type ParameterMode =
    | Position of int
    | Immediate of int with
    static member parse pNum = function
        | 0 -> Position(pNum)
        | 1 -> Immediate(pNum)
        | _ -> failwith("Invalid Number")

type InstructionDescriptor =
    | MultiplyInstruction      of ParameterMode * ParameterMode * ParameterMode
    | AddInstruction           of ParameterMode * ParameterMode * ParameterMode
    | InputInstruction         of ParameterMode
    | OutputInstruction        of ParameterMode
    | JumpIfTrueInstruction    of ParameterMode * ParameterMode
    | JumpIfFalseInstruction   of ParameterMode * ParameterMode
    | LessThenInstruction      of ParameterMode * ParameterMode * ParameterMode
    | EqualsInstruction        of ParameterMode * ParameterMode * ParameterMode
    | FinishedInstruction
  with 
    static member parse value = 
        let opCode = value % 100
        let p1 = value / 100 % 10   |> ParameterMode.parse 1
        let p2 = value / 1000 % 10  |> ParameterMode.parse 2
        let p3 = value / 10000 % 10 |> ParameterMode.parse 3
        match opCode with
        | 1 -> AddInstruction           (p1, p2, p3)
        | 2 -> MultiplyInstruction      (p1, p2, p3)
        | 3 -> InputInstruction         (p1)
        | 4 -> OutputInstruction        (p1)
        | 5 -> JumpIfTrueInstruction    (p1, p2)
        | 6 -> JumpIfFalseInstruction   (p1, p2)
        | 7 -> LessThenInstruction      (p1, p2, p3)
        | 8 -> EqualsInstruction        (p1, p2, p3)
        | 99 -> FinishedInstruction
        | _ -> failwithf "Invalid op code: %i" opCode
     
type InstructionObject =
    | Multiply          of int * int * (int -> unit)
    | Add               of int * int * (int -> unit)
    | Input             of (int -> unit)
    | Output            of (unit -> int)
    | JumpIfTrue        of int * int
    | JumpIfFalse       of int * int
    | LessThen          of int * int * (int -> unit)
    | Equals            of int * int * (int -> unit)
    | Finished
    with 
    static member Create (buffer:int array) pos (oc:InstructionDescriptor) =
        let getValue = function
            | Position  pNum -> buffer.[buffer.[pos + pNum]]
            | Immediate pNum -> buffer.[pos + pNum]
        let setValue pMode value = 
            match pMode with
            | Position  pNum -> buffer.[buffer.[pos + pNum]] <- value
            | Immediate pNum -> buffer.[pos + pNum] <- value

        match oc with
            | MultiplyInstruction    (a, b, c) -> Multiply    (getValue a, getValue b, setValue c)
            | AddInstruction         (a, b, c) -> Add         (getValue a, getValue b, setValue c)
            | InputInstruction       (a)       -> Input       (fun x -> setValue a x)
            | OutputInstruction      (a)       -> Output      (fun () -> (getValue a))
            | JumpIfTrueInstruction  (a, b)    -> JumpIfTrue  (getValue a, getValue b)
            | JumpIfFalseInstruction (a, b)    -> JumpIfFalse (getValue a, getValue b)
            | LessThenInstruction    (a, b, c) -> LessThen    (getValue a, getValue b, setValue c)
            | EqualsInstruction      (a, b, c) -> Equals      (getValue a, getValue b, setValue c)
            | FinishedInstruction              -> Finished

type ComputeResult =
    | OutputSignal of int
    | FinishedSignal

type Computer(buffer : int array, phase: int, name: char) =
     let mutable initial = true
     member val Buffer = buffer with get
     member val Name = name with get
     member val Phase = phase with get
     member val Position = 0 with get, set
     member val Output = 0 with get, set
     
     member this.Compute (signal: int) =
         let initialList = (if initial then [this.Phase; signal ] else [ signal ])
         initial <- false
         let rec compute' (input': int list) =        
             let instructionDescriptor = this.Buffer.[this.Position] |> InstructionDescriptor.parse
             let instructionObject = instructionDescriptor |> InstructionObject.Create buffer this.Position
             match instructionObject with
                 | Multiply    (x, y, setter) -> setter (x * y); this.Position <- this.Position + 4; compute' input'
                 | Add         (x, y, setter) -> setter (x + y); this.Position <- this.Position + 4; compute' input'
                 | Input       (action)       -> action(input'.Head); this.Position <- this.Position + 2; compute' input'.Tail
                 | Output      (action)       -> this.Output <- action(); this.Position <- this.Position + 2; OutputSignal(this.Output)
                 | JumpIfTrue  (a, b)         -> this.Position <- (if a <> 0 then b else this.Position + 3); compute' input'
                 | JumpIfFalse (a, b)         -> this.Position <- (if a = 0 then b else this.Position + 3); compute' input'
                 | LessThen    (a, b, setter) -> setter(if a < b then 1 else 0); this.Position <- this.Position + 4; compute' input'
                 | Equals      (a, b, setter) -> setter(if a = b then 1 else 0); this.Position <- this.Position + 4; compute' input'
                 | Finished                   -> FinishedSignal         
         compute' initialList

let rec processAmplifierPart2 (computers: Computer list)=
    (* initial phase *)
    let computation = seq {
        let mutable singalInput = 0
        let mutable finalOutput = 0
        for computer in computers |> Seq.cycle do
            printfn "Computer %c with phase %i" computer.Name computer.Phase
            match (computer.Compute singalInput) with
            | OutputSignal output -> 
                singalInput <- output;
                finalOutput <- if computer.Name = 'E' then computer.Output else finalOutput
            | FinishedSignal -> yield finalOutput }

    computation |> Seq.head

let rec processAmplifierPart1 (computers: Computer list) =
    let mutable singalInput = 0
    for computer in computers do
        printf "Computer %c with phase %i" computer.Name computer.Phase 
        match (computer.Compute singalInput) with
        | OutputSignal output -> singalInput <- output;
        | FinishedSignal -> failwith("Shout not happen")
    singalInput
    

let rec distribute e = function
| [] -> [[e]]
| x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
| [] -> [[]]
| e::xs -> List.collect (distribute e) (permute xs)

let createComputers input phase =
    phase |> List.mapi (fun i p -> Computer(Array.copy input, p, (i+65) |> char))

// Part 1
//[0;1;2;3;4] |> permute |> List.map (createComputers input) |> List.map processAmplifierPart1 |> List.max

// Part 2
//[5;6;7;8;9] |> permute |> List.map (createComputers input) |> List.map processAmplifierPart2 |> List.max
