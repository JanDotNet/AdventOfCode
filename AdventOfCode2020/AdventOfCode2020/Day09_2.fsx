open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day09.txt")

let allNumbers = File.ReadAllLines(file) |> Array.map int64
let target = 10884537L

let findContiguousRange num (numbers:int64 array) =
    let rec findContiguousRange' (idx, length) =
        let range = numbers.[idx..length]
        let sum = range |> Array.sum
        if sum = num then
            range
        elif sum < num then 
            findContiguousRange' (idx, length + 1)
        else 
            findContiguousRange' (idx + 1, 2)
    findContiguousRange' (0, 2)

let contiguousRange = allNumbers |> findContiguousRange target
let result = (contiguousRange |> Array.min) + (contiguousRange |> Array.max)