open System
open System.IO
open System.Globalization

let calcPowerLevel serialNumber x y =
    let rackId = x + 10
    let r2 = rackId * y
    let r3 = r2 + serialNumber
    let r4 = r3 * rackId
    let r5 = (r4 / 100) % 10
    r5 - 5

let generatePositions xStart yStart squareSize = 
    seq { for x in xStart..(xStart + squareSize - 1) do
          for y in yStart..(yStart + squareSize - 1) do
          yield (x, y) }

let calculateSquare squareSize serialNumber x y =
    generatePositions x y squareSize
    |> Seq.map (fun (x, y) -> calcPowerLevel serialNumber x y)
    |> Seq.sum    

let solve serialNumber boardSize squareSize = 
    let calcSquare = calculateSquare squareSize serialNumber
    generatePositions 1 1 (boardSize - squareSize)
    |> Seq.map (fun (x, y) -> (calcSquare x y, x, y))
    |> Seq.maxBy (fun (score, _, _) -> score)

let a = solve 6548 300 3