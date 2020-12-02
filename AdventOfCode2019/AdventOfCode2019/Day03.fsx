open System
open System.IO
open System.Collections.Generic

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day03.txt")

let lines = File.ReadAllLines(file)
let wire1 = lines.[0].Split(',') |> Array.toList
let wire2 = lines.[1].Split(',') |> Array.toList

type Position = { X: int; Y: int } with
    member this.up() = { this with Y = this.Y + 1 }
    member p.down() = { p with Y = p.Y - 1 }
    member p.right() = { p with X = p.X + 1 }
    member p.left() = { p with X = p.X - 1 }

type PositionDetail = { Position:Position; StepCount: int}

type Instruction = { Direction: char; Distance: int } with
    member this.Transform (pos:PositionDetail) =
        match this.Direction with
        | 'L' -> { pos with Position = pos.Position.left(); StepCount = pos.StepCount + 1 }
        | 'R' -> { pos with Position = pos.Position.right(); StepCount = pos.StepCount + 1 }
        | 'U' -> { pos with Position = pos.Position.up(); StepCount = pos.StepCount + 1 }
        | 'D' -> { pos with Position = pos.Position.down(); StepCount = pos.StepCount + 1 }
        |  x  -> failwith (sprintf "Invalid instruction direction: %c" x)

    
let toInstructions = List.map (fun (x:string) -> { Direction = x.[0]; Distance = x.Substring(1) |> int})
let instructions1 = wire1 |> toInstructions
let instructions2 = wire2 |> toInstructions

let generatePositions instructions =    
    let startPosition = { Position = { X = 0; Y = 0 }; StepCount = 0 }    
    
    let instructionToPositions (instruction:Instruction) (positions:PositionDetail list) =
        let folder (acc:PositionDetail list) x =
            let newPos = instruction.Transform acc.Head
            newPos :: acc
        List.fold folder positions [1 .. instruction.Distance]

    let folder acc x =
        let newPositions = instructionToPositions x acc
        newPositions
    List.fold folder [ startPosition ] instructions

let getPositions = generatePositions >>
                   List.groupBy (fun x -> x.Position) >>
                   List.map (fun (pos, steps) -> { Position = pos; StepCount = steps |> List.map (fun x -> x.StepCount) |> List.min })

let w1 = instructions1 |> getPositions
let w2 = instructions2 |> getPositions

let s1 = w1 |> List.map (fun x -> x.Position) |> Set.ofList
let s2 = w2 |> List.map (fun x -> x.Position) |> Set.ofList
let crossPositions = s1 |> Set.intersect s2

let all = s1 |> Set.union s2 |> Set.toList
printfn "Min: %i %i" (all |> List.minBy (fun x -> x.X)).X (all |> List.minBy (fun x -> x.Y)).Y
printfn "Max: %i %i" (all |> List.maxBy (fun x -> x.X)).X (all |> List.maxBy (fun x -> x.Y)).Y

// part one
let minDistance = (crossPositions |> Seq.map (fun p -> Math.Abs(p.X) + Math.Abs(p.Y)) |> Seq.filter (fun x -> x <> 0) |> Seq.min)
printfn "Min. Distance: %i" minDistance 

let pos1 = w1 |> List.filter (fun x -> crossPositions.Contains(x.Position)) |> List.sortBy (fun x -> x.Position)
let pos2 = w2 |> List.filter (fun x -> crossPositions.Contains(x.Position)) |> List.sortBy (fun x -> x.Position)

let posPairs = pos1 |> List.zip pos2
let minStepCount = (posPairs |> Seq.map (fun (l, r) -> l.StepCount + r.StepCount) |> Seq.filter (fun x -> x <> 0) |> Seq.min)
printfn "Min. StepCount: %i" minStepCount 



