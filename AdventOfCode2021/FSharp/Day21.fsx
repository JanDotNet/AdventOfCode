open System.IO
open System

module Seq = 
  let repeat items = 
    seq { while true do yield! items }

let mutable posP1 = 10
let mutable posP2 = 6
let mutable scoreP1 = 0
let mutable scoreP2 = 0
type Player = Player1 | Player2
    with static member other = function
                                | Player1 -> Player2
                                | Player2 -> Player1
let mutable current = Player1
let mutable count = 0

let diceSeq = Seq.repeat [1..3] 
                |> Seq.chunkBySize 3 
                |> Seq.map (fun x -> x |> Array.sum)

let play pos score dice =
    let pos' = (pos + dice) % 10
    let pos'' = if pos' = 0 then 10 else pos'
    let score' = score + pos''
    pos'', score'

seq { for dice in diceSeq do        
        if (scoreP1 >= 21 || scoreP1 >= 21)
        then yield (min scoreP1 scoreP2) * count
        else
            count <- count + 3
            match current with
            | Player1 -> let pos, score = play posP1 scoreP1 dice
                         posP1 <- pos
                         scoreP1 <- score
                         current <- Player2
            | Player2 -> let pos, score = play posP2 scoreP2 dice
                         posP2 <- pos
                         scoreP2 <- score
                         current <- Player1
} |> Seq.head

type UniverseState = {Count:int64; PosP1:int; PosP2:int; ScoreP1:int; ScoreP2:int}


let rec nextStep (states:UniverseState list) currentPlayer =
    let moves = [(3,1L); (4,3L); (5,6L); (6,7L); (7,6L); (8,3L); (9, 1L)] 
    let folder (states:UniverseState list) (us:UniverseState) =
        let statesNew = moves |> List.map (fun (dice, count) -> match currentPlayer with
                                                                | Player1 -> let pos, score = play us.PosP1 us.ScoreP1 dice
                                                                             {us with Count = us.Count * count; 
                                                                                      PosP1 = pos;
                                                                                      ScoreP1 = score }
                                                                | Player2 -> let pos, score = play us.PosP2 us.ScoreP2 dice
                                                                             {us with Count = us.Count * count; 
                                                                                      PosP2 = pos;
                                                                                      ScoreP2 = score })
        states |> List.append statesNew
    states |> List.fold folder []

let compress universes =
    universes |> List.groupBy (fun u -> (u.PosP1, u.ScoreP1, u.PosP2, u.ScoreP2))
              |> List.map (fun g -> let posP1, scoreP1, posP2, scoreP2 = g |> fst
                                    let count = g |> snd |> List.map (fun x -> x.Count) |> List.sum
                                    {PosP1 = posP1; ScoreP1 = scoreP1; PosP2 = posP2; ScoreP2 = scoreP2; Count = count})



let initial = { Count=1; PosP1=10; PosP2=6; ScoreP1=0; ScoreP2=0 }

let rec run universesPlaying universesFinished player =
    let universes' = nextStep universesPlaying player |> compress
    let player' = player |> Player.other
    let universesPlaying' = universes' |> List.filter (fun u -> u.ScoreP1 < 21 && u.ScoreP2 < 21)
    let universesFinished' = universesFinished |> List.append (universes' |> List.filter (fun u -> u.ScoreP1 >= 21 || u.ScoreP2 >= 21))
    printfn "universes playing %i:" universesPlaying'.Length
    if universesPlaying'.Length = 0
    then let winsP1 = universesFinished' |> List.filter (fun u -> u.ScoreP1 > u.ScoreP2) |> List.map (fun u -> u.Count |> bigint) |> List.sum
         let winsP2 = universesFinished' |> List.filter (fun u -> u.ScoreP1 < u.ScoreP2) |> List.map (fun u -> u.Count |> bigint) |> List.sum
         max winsP1 winsP2
    else run universesPlaying' universesFinished' player'

run [initial] [] Player1