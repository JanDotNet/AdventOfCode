open System

let parseInt str =
    match Int32.TryParse str with
    | true, num -> Some num
    | false, _ -> None

"123" |> parseInt
    
let mul a b = a * b
let mulOpt = Option.map2 mul

let parseIntOpt = Option.bind parseInt

