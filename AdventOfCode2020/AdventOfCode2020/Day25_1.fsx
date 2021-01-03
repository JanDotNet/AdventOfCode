open System
open System.IO

let handshakeStep subjectNumber value =
  let value' = value * subjectNumber
  value' % 20201227UL

let handshake subjectNumber loopSize =
  let rec handshake' (value:uint64) loop =
    if loop = 0
    then value
    else
      let value' = handshakeStep value subjectNumber
      handshake' value' (loop - 1)
  handshake' 1UL loopSize

let bruteforce subjectNum targetValue =
  seq {
    let mutable value = 1UL
    while true do
      value <- handshakeStep 7UL value
      yield value
  } |> Seq.mapi (fun i v -> (i, v))
    |> Seq.find (fun (i, v) -> v = targetValue )
    |> fst |> (+) 1

let pkey_card = 8184785UL
let pkey_door = 5293040UL

let loopSize_card = bruteforce 7UL pkey_card
let loopSize_door = bruteforce 7UL pkey_door

let key_card = handshake pkey_card loopSize_door
let key_door = handshake pkey_door loopSize_card