open System
open System.IO
open System.Collections.Generic

let a = 1 + 1

let toNumber x1 x2 x3 x4 x5 x6 = 
    x1 * 100000 + x2 * 10000 + x3 * 1000 + x4 * 100 + x5 * 10 + x6

// 372037-905157
let numbers = seq {
    for d1 in 3..9 do
        for d2 in d1..9 do
            for d3 in d2..9 do
                for d4 in d3..9 do
                    for d5 in d4..9 do
                        for d6 in d5..9 do
                            let num = toNumber d1 d2 d3 d4 d5 d6
                            //let hasDuplicates = [d1; d2; d3; d4; d5; d6] |> List.distinct |> List.length < 6
                            let hasDuplicatesPrat2 =
                                (d1 = d2 && d1 <> d3) ||
                                (d2 = d3 && d2 <> d4 && d2 <> d1) ||
                                (d3 = d4 && d3 <> d5 && d3 <> d2) ||
                                (d4 = d5 && d4 <> d6 && d4 <> d3) ||
                                (d5 = d6 && d4 <> d5)

                            let isInRange = num > 372037 && num < 905157
                            if hasDuplicatesPrat2 && isInRange then
                                yield num
}

printfn "%i" (numbers |> Seq.length)


type State =
    | SingleDigit of int
    | DoubleDigit of int
    | Final of int
    | ManyDigits of int
    | Invalid
    
let stateTransition state next =
    match state with
    | SingleDigit prev when prev = next -> DoubleDigit(next)
    | SingleDigit prev when prev < next -> SingleDigit(next)
    | DoubleDigit prev when prev < next -> Final(next)
    | DoubleDigit prev when prev = next -> ManyDigits(next)
    | Final prev       when prev <= next -> Final(next)    
    | ManyDigits prev  when prev = next -> ManyDigits(next)
    | ManyDigits prev  when prev < next -> SingleDigit(next)
    | Invalid -> Invalid
    | _ -> Invalid

let success = function
    | DoubleDigit _ -> true
    | Final _ -> true
    | _ -> false
    
let toDigits (x:int) = x |> (string >> Seq.toList >> List.map (string >> int))
let toState x = List.fold stateTransition (SingleDigit(0)) x   

[372037..905157]
    |> List.filter (toDigits >> toState >> success)
    |> List.length

