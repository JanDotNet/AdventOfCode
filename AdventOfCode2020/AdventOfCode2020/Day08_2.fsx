open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day08.txt")

type Instruction =
    | Acc of int
    | Nop of int
    | Jmp of int

let parseInstructions (line:string) =
    let values = line.Split(' ')
    let num = values.[1] |> int
    match values.[0] with
    | "acc" -> Acc(num)
    | "nop" -> Nop(num)
    | "jmp" -> Jmp(num)
    | _ -> failwith "invalid operation"

let swap = function
    | Acc x -> Acc x
    | Nop n -> Jmp n
    | Jmp n -> Nop n

let run (program:Instruction array) =
    let rec run' (idx, acc, processed) (program:Instruction array) =
        let progLength = program |> Array.length
        if idx = progLength then
            // valid termination
            Some(acc)
        else if processed |> Set.contains idx || idx > progLength || idx < 0 then
            // invalid termination or infinity loop
            None
        else
            let processed' = processed |> Set.add idx
            match program.[idx] with
            | Acc num -> program |> run' (idx + 1, acc + num, processed')
            | Nop num -> program |> run' (idx + 1, acc, processed')
            | Jmp num -> program |> run' (idx + num, acc, processed')
    run' (0, 0, Set.empty) program

let tryFix program fixIdx = 
    program |> Array.mapi (fun idx inst -> if idx = fixIdx then inst |> swap else inst )

let program = File.ReadAllLines(file) |> Array.map parseInstructions
let upperIdx = program.Length - 1
let result = [ 0 .. upperIdx]
             |> List.map (tryFix program)
             |> List.map run
             |> List.filter Option.isSome



