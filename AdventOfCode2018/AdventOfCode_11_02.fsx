open System
open System.IO
open System.Globalization
open System.Collections.Generic

let memoize2D size invalidValue f =
  let cache = Array2D.create size size invalidValue
  fun x y ->
    match cache.[x,y] with
    | value when value <> invalidValue -> value
    | _ ->
        let value = f x y
        cache.[x, y] <- value
        value

let calcPowerLevel serialNumber x y =
    let rackId = x + 10
    let r2 = rackId * y
    let r3 = r2 + serialNumber
    let r4 = r3 * rackId
    let r5 = (r4 / 100) % 10
    r5 - 5

let generatePositions xStart yStart size = 
    seq { for x in xStart..(xStart + size - 1) do
          for y in yStart..(yStart + size - 1) do
          yield (x, y) }

let calculateSquare solver squareSize x y =
    generatePositions x y squareSize
    |> Seq.map (fun (x, y) -> solver x y)
    |> Seq.sum    

let solve solver boardSize squareSize = 
    let calcSquare = calculateSquare solver squareSize
    generatePositions 1 1 (boardSize - squareSize)
    |> Seq.map (fun (x, y) -> (calcSquare x y, x, y))
    |> Seq.maxBy (fun (score, _, _) -> score)

(* INPUT *)
let boardSize = 300
let serialNumber = 6548
let solver = memoize2D boardSize 0 (calcPowerLevel serialNumber)

(* LOGIC *)
let solveForSquareSize = solve solver boardSize
let r = seq { for x in [1..299] do yield x }
        |> Seq.map solveForSquareSize
        |> Seq.maxBy (fun (score, _, _) -> score)