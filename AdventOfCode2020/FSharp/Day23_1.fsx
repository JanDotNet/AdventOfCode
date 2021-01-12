open System
open System.IO

module Seq = 
  let rec cycle xs = seq { yield! xs; yield! cycle xs }

let getCups input = 
  input |> List.ofSeq |> List.map (string >> int)

let getNextThree current cups =
  cups |> Seq.cycle |> Seq.skipWhile ((<>) current) |> Seq.skip 1 |> Seq.take 3 |> Seq.toList

let findDestination current cups =
  let sortedCups = cups |> List.sort
  let index = sortedCups |> List.findIndex ((=) current)
  if index > 0 then sortedCups.[index-1] else sortedCups |> List.max

let mergeCups cupsToMerge destination cups =
  let destIndex = cups |> List.findIndex ((=) destination)
  let left = cups.[0..destIndex]  
  let right = cups.[destIndex+1..]
  left @ cupsToMerge @ right

let getNextCurrent current cups =
  cups |> Seq.cycle |> Seq.skipWhile ((<>) current) |> Seq.skip 1 |> Seq.head

let rec playOneRound (current, cups) round =
  let threeCups = cups |> getNextThree current
  let remainingCups = cups |> List.except threeCups
  let destination = remainingCups |> findDestination current
  let cups' = remainingCups |> mergeCups threeCups destination
  let current' = cups' |> getNextCurrent current
  printfn "Round: %i CurrentCup: %i AllCups: %A Destination: %i PickUp: %A" round current cups destination threeCups
  current', cups'

let sortRawResult raw =
  raw |> Seq.cycle |> Seq.skipWhile ((<>) 1) |> Seq.skip 1 |> Seq.takeWhile ((<>) 1) |> Seq.toList

let cups = "389125467" |> getCups
let x = [1..100] |> List.fold playOneRound (6, cups) |> snd
let result = x |> sortRawResult |> List.map string |> String.concat ""
