open System
open System.IO
open System.Collections.Generic

// target area: x=241..273, y=-97..-63
let xMin, xMax, yMin, yMax = 241, 273, -97, -63

// target area: x=20..30, y=-10..-5
//let xMin, xMax, yMin, yMax = 20, 30, -10, -5

let center = (xMax + xMin) / 2, (yMax + yMin) / 2
let start = (0,0)

let isInTargetArea (x, y) = x >= xMin && x <= xMax && y >= yMin && y <= yMax
let nextPosition (vel_x, vel_y) (pos_x, pos_y) = (pos_x + vel_x, pos_y + vel_y)
let nextVelocity (vel_x, vel_y) = 
    let vel_x' = if vel_x > 0 then vel_x - 1
                 elif vel_x < 0 then vel_x + 1
                 else 0
    let vel_y' = vel_y - 1
    (vel_x', vel_y')
let isAboveArea (x, y) = x <= xMax && y >= yMin
let getDistance (x1, y1) (x2, y2) = sqrt((pown (x2-x1) 2) + (pown (y2-y1) 2) |> float)

let findPositions vel =
    let rec findPositions' vel pos positions yTop =
        let isInTarget = pos |> isInTargetArea
        let positions' = if isInTarget then pos::positions else positions
        let yTop' = if snd pos > yTop then snd pos else yTop
        let pos' = pos |> nextPosition vel
        let vel' = vel |> nextVelocity
        let distance = getDistance center pos
        let distance' = getDistance center pos'
        if isInTarget || pos |> isAboveArea
        then findPositions' vel' pos' positions' yTop'
        else positions, yTop'
    findPositions' vel start [] 0

let bruteForceVelocities =
    seq {
        for x in 1..xMax do
            for y in yMin..200 do
                yield (x, y)
    }
bruteForceVelocities |> Seq.toList
let solve1 = bruteForceVelocities 
                |> Seq.map (fun vel -> vel, vel |> findPositions)
                |> Seq.filter (fun (vel, (pos, yTop)) -> pos |> List.length > 0)
                |> Seq.maxBy (fun (vel, (pos, yTop)) -> yTop)
                |> (snd >> snd)
let solve2 = bruteForceVelocities 
                |> Seq.map (fun vel -> vel, vel |> findPositions)
                |> Seq.filter (fun (vel, (pos, yTop)) -> pos |> List.length > 0)
                |> Seq.length

printfn "Solution 1: %i" solve1
printfn "Solution 2: %i" solve2