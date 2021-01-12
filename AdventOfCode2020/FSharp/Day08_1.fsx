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

let run (program:Instruction array) =
    let rec run' (idx, acc, processed) (program:Instruction array) =
        if processed |> Set.contains idx then
            acc
        else
            let processed' = processed |> Set.add idx
            match program.[idx] with
            | Acc num -> program |> run' (idx + 1, acc + num, processed')
            | Nop num -> program |> run' (idx + 1, acc, processed')
            | Jmp num -> program |> run' (idx + num, acc, processed')
    run' (0, 0, Set.empty) program

let program = File.ReadAllLines(file) |> Array.map parseInstructions

let result = program |> run