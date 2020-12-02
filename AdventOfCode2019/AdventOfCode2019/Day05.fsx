open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day05.txt")

let input = File.ReadAllText(file).Split(',') |> Array.map int

let inputCode = 5

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
    | Input             of (unit -> unit)
    | Output            of (unit -> unit)
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
            | InputInstruction       (a)       -> Input       (fun () -> setValue a inputCode)
            | OutputInstruction      (a)       -> Output      (fun () -> printfn "output: %i" (getValue a))
            | JumpIfTrueInstruction  (a, b)    -> JumpIfTrue  (getValue a, getValue b)
            | JumpIfFalseInstruction (a, b)    -> JumpIfFalse (getValue a, getValue b)
            | LessThenInstruction    (a, b, c) -> LessThen    (getValue a, getValue b, setValue c)
            | EqualsInstruction      (a, b, c) -> Equals      (getValue a, getValue b, setValue c)
            | FinishedInstruction              -> Finished

(* Procoessing Logic *)
let processPosition (buffer:int array) pos =
    let instructionDescriptor = buffer.[pos] |> InstructionDescriptor.parse
    let instructionObject = instructionDescriptor |> InstructionObject.Create buffer pos
    match instructionObject with
        | Multiply    (x, y, setter) -> setter (x * y); Some(pos + 4)
        | Add         (x, y, setter) -> setter (x + y); Some(pos + 4)
        | Input       (action)       -> action(); Some(pos + 2)
        | Output      (action)       -> action(); Some(pos + 2)
        | JumpIfTrue  (a, b)         -> Some(if a <> 0 then b else pos + 3)
        | JumpIfFalse (a, b)         -> Some(if a = 0 then b else pos + 3)
        | LessThen    (a, b, setter) -> setter(if a < b then 1 else 0); Some(pos + 4)
        | Equals      (a, b, setter) -> setter(if a = b then 1 else 0); Some(pos + 4)
        | Finished                   -> None

let rec processPositionLoop (buffer:int array) pos =
    let result = processPosition buffer pos
    
    match result with
        | Some newPos -> processPositionLoop buffer newPos
        | None        -> printfn "Finished"

processPositionLoop input 0