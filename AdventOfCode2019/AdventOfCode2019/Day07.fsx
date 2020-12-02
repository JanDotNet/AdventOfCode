open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day07.txt")

let input = File.ReadAllText(file).Split(',') |> Array.map int

let inputp1e1 = [|3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0|]
let inputp1e2 = [|3;23;3;24;1002;24;10;24;1002;23;-1;23;101;5;23;23;1;24;23;23;4;23;99;0;0|]
let inputp2e1 = [|3;26;1001;26;-4;26;3;27;1002;27;2;27;1;27;26;27;4;27;1001;28;-1;28;1005;28;6;99;0;0;5|]

(*Helper*)
module Seq = let rec cycle xs = seq { yield! xs; yield! cycle xs }

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

//type ExecutionContext = {Position:int; Input:int list; Output: int list}

type ExecutionContext2 = {Position:int; PhaseSettings: int seq; UsePhaseSetting: bool; Output:int}
    with 
        member this.NextInput() =
                match this.UsePhaseSetting with
                | true  -> this.PhaseSettings |> Seq.head
                | false -> this.Output

(* Procoessing Logic *)
//let compute (buffer:int array) (ctx:ExecutionContext) =
//    let rec compute' (buffer:int array) (ctx:ExecutionContext) =        
//        let instructionDescriptor = buffer.[ctx.Position] |> InstructionDescriptor.parse
//        let instructionObject = instructionDescriptor |> InstructionObject.Create buffer ctx.Position
//        let compBuf = compute' buffer
//        match instructionObject with
//            | Multiply    (x, y, setter) -> setter (x * y); compBuf {ctx with Position = ctx.Position + 4}
//            | Add         (x, y, setter) -> setter (x + y); compBuf {ctx with Position = ctx.Position + 4}
//            | Input       (action)       -> action(ctx.Input.Head); compBuf {ctx with Position = ctx.Position + 2; Input = ctx.Input.Tail}
//            | Output      (action)       -> compBuf {ctx with Position = ctx.Position + 2; Output = action() :: ctx.Output}
//            | JumpIfTrue  (a, b)         -> compBuf {ctx with Position = if a <> 0 then b else ctx.Position + 3}
//            | JumpIfFalse (a, b)         -> compBuf {ctx with Position = if a = 0 then b else ctx.Position + 3}
//            | LessThen    (a, b, setter) -> setter(if a < b then 1 else 0); compBuf {ctx with Position = ctx.Position + 4}
//            | Equals      (a, b, setter) -> setter(if a = b then 1 else 0); compBuf {ctx with Position = ctx.Position + 4}
//            | Finished                   -> ctx
//    compute' buffer ctx

let computePart2 (buffer:int array) (ctx:ExecutionContext2) =
    let rec compute' (buffer:int array) (ctx:ExecutionContext2) =        
        let instructionDescriptor = buffer.[ctx.Position] |> InstructionDescriptor.parse
        let instructionObject = instructionDescriptor |> InstructionObject.Create buffer ctx.Position
        let compBuf = compute' buffer
        match instructionObject with
            | Multiply    (x, y, setter) -> setter (x * y); 
                                            compBuf {ctx with Position = ctx.Position + 4}
            | Add         (x, y, setter) -> setter (x + y);
                                            compBuf {ctx with Position = ctx.Position + 4}
            | Input       (action)       -> action(ctx.NextInput());
                                            compBuf {ctx with Position = ctx.Position + 2; UsePhaseSetting = false}
            | Output      (action)       -> compBuf {ctx with Position = ctx.Position + 2; Output = action(); UsePhaseSetting = true; PhaseSettings = ctx.PhaseSettings |> Seq.skip 1}
            | JumpIfTrue  (a, b)         -> compBuf {ctx with Position = if a <> 0 then b else ctx.Position + 3}
            | JumpIfFalse (a, b)         -> compBuf {ctx with Position = if a = 0 then b else ctx.Position + 3}
            | LessThen    (a, b, setter) -> setter(if a < b then 1 else 0); 
                                            compBuf {ctx with Position = ctx.Position + 4}
            | Equals      (a, b, setter) -> setter(if a = b then 1 else 0); 
                                            compBuf {ctx with Position = ctx.Position + 4}
            | Finished                   -> ctx
    compute' buffer ctx    

//let rec processAmplifierPart1 (buffer:int array) phaseSequence =
//    let rec processAmplifier' remPhases out = 
//        let bufferClone = Array.copy buffer
//        match remPhases with
//        | h::t -> 
//            let ctx = compute bufferClone {Position = 0; Input = [h; out]; Output = []};
//            processAmplifier' t (ctx.Output |> List.exactlyOne)
//        | []   -> out
//    processAmplifier' phaseSequence 0

let rec processAmplifierPart2 (buffer:int array) phaseSequence =
    let bufferClone = Array.copy buffer
    let ctx = computePart2 bufferClone { Position = 0; PhaseSettings = phaseSequence; UsePhaseSetting = true; Output = 0 }
    ctx

let rec distribute e = function
| [] -> [[e]]
| x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
| [] -> [[]]
| e::xs -> List.collect (distribute e) (permute xs)

//[0..4] |> permute |> List.map (processAmplifierPart1 input) |> List.max

//[5..9] |> permute |> List.map (processAmplifierPart2 inputp2e1) |> List.max

[9;8;7;6;5] |> (processAmplifierPart2 inputp2e1)



let txt1 = "ehTsii  s aemssga"
let txt2 = "This is second message"

printfn "%i" (txt1 |> Seq.length)

