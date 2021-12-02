open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day02.txt")

//let input = ["forward 5";"down 5";"forward 8";"up 3";"down 8";"forward 2"]
let input = File.ReadAllLines(file)

    
let applyCmd1 (x, y) (cmd:string) =
    let direction = cmd.Split(' ').[0]
    let length = int(cmd.Split(' ').[1])
    match direction with
        | "forward" -> (x+length, y)
        | "down" -> (x, y+length)
        | "up" -> (x, y-length)
        | _ -> failwith("Invalid direction")

let applyCmd2 (x, y, a) (cmd:string) =
    let direction = cmd.Split(' ').[0]
    let length = int(cmd.Split(' ').[1])
    match direction with
        | "forward" -> (x+length, y + a * length, a)
        | "down" -> (x, y, a+length)
        | "up" -> (x, y, a-length)
        | _ -> failwith("Invalid direction")


let solve1 cmds =
    cmds
    |> Seq.fold applyCmd1 (0, 0)
    |> fun (x, y) -> x * y

let solve2 cmds =
    cmds
    |> Seq.fold applyCmd2 (0, 0, 0)
    |> fun (x, y, a) -> x * y

printfn "Solution 1: %i" (solve1 input)
printfn "Solution 2: %i" (solve2 input)