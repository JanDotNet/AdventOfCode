open System
open System.IO
open System.Globalization

let root = __SOURCE_DIRECTORY__
let inputFile = root + @"\data\adventofcode_05.txt"
let input = File.ReadAllLines(inputFile) |> Seq.head;

let input2 = "dabAcaACaCBAcCcaDA"

let check a b =
    let a1 = a |> int
    let a2 = b |> int
    (a1 - a2) |> abs = 32

let folder (lastItemRemoved, lastItem, lst : char list) (a, b) =
    match (lastItemRemoved, check a b) with
    | (true, _)     -> (false, b, lst)
    | (_, true)     -> (true, b, lst)
    | (_, false)    -> (false, b, a :: lst)

let fixLastItem (lastItemRemoved, lastItem, lst : char list) =
    match (lastItemRemoved) with
        | true -> lst
        | false -> lastItem :: lst        

let proc i = i
            |> Seq.pairwise
            |> Seq.fold folder (false, ' ', [])
            |> fixLastItem
            |> List.rev

let rec procRec (i:char list) =
    let cnt = i.Length
    let procLst = i |> proc
    if (procLst.Length = cnt) 
    then procLst 
    else procRec procLst

let getSeq (r1, r2) (i: char seq) =
    i |> Seq.filter (fun x -> x <> r1 && x <> r2)

let getChars =
    ['a' .. 'z'] |> List.map (fun x -> (x, Char.ToUpper x))

let getLength seq = seq |> Seq.toList |> procRec |> List.length

let getLentghForCharPair lst charPair =
    let length = lst |> getSeq charPair |> getLength
    (fst charPair, snd charPair, length)

let getResults = getChars 
                |> Seq.map (getLentghForCharPair "dabAcCaCBAcCcaDA") 
                |> Seq.sortByDescending (fun (_, _, cnt) -> cnt)
                |> Seq.toList