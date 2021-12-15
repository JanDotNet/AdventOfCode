open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day12.txt")

let input2 = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"

//let input = input2.Split('\n') |> List.ofArray

let input = File.ReadAllLines(file) |> List.ofArray


// printfn "Solution 1: %i" (input |> solve1)
// printfn "Solution 2: %i" (input |> solve2)