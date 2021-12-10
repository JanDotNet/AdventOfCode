open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day10.txt")

// let input = "[({(<(())[]>[[{[]{<()<>>
// [(()[<>])]({[<{<<[]>>(
// {([(<{}[<>[]}>{[]{[(<()>
// (((({<>}<{<{<>}{[]{[]{}
// [[<[([]))<([[{}[[()]]]
// [{[{({}]{}}([{[{{{}}([]
// {<[[]]>}<{[{[{[]{()[[[]
// [<(<(<(<{}))><([]([]()
// <{([([[(<>()){}]>(<<{{
// <{([{{}}[<[[[<>{}]]]>[]]".Split('\n') |> List.ofArray

let input = File.ReadAllLines(file) |> List.ofArray

let pairs = [('[', ']'); ('(', ')'); ('<', '>'); ('{', '}')]
let opens = pairs |> List.map fst

let rec processLinePart1 (opensStack:char list) (line:char list) =
    if line.Length = 0 
        then 0
    elif opensStack.Length = 0 
        then processLinePart1 [line.Head] line.Tail
    elif opens |> List.contains line.Head 
        then processLinePart1 (line.Head::opensStack) line.Tail
    else match line.Head with
         | ')' when opensStack.Head <> '(' -> 3
         | ']' when opensStack.Head <> '[' -> 57
         | '}' when opensStack.Head <> '{' -> 1197
         | '>' when opensStack.Head <> '<' -> 25137
         | _ -> processLinePart1 opensStack.Tail line.Tail

let rec scoreBrackets (total:int64) (line:char list) =
    let scoreBracket = function
                       | ')' -> 1L | ']' -> 2L 
                       | '}' -> 3L | '>' -> 4L 
                       | _ -> failwith("test")    
    if line.Length = 0 
    then total
    else scoreBrackets (total * 5L + (scoreBracket line.Head)) line.Tail

let rec processLinePart2 (lastOpen:char list) (line:char list) =
    if line.Length = 0
    then let pm = pairs |> Map.ofList
         lastOpen |> List.map (fun x -> pm[x]) |> (scoreBrackets 0L)
    elif lastOpen.Length = 0 then processLinePart2 [line.Head] line.Tail
    elif opens |> List.contains line.Head then processLinePart2 (line.Head::lastOpen) line.Tail
    elif pairs |> List.contains (lastOpen.Head, line.Head) then processLinePart2 lastOpen.Tail line.Tail
    else processLinePart2 lastOpen.Tail line.Tail

let solve1 i = i |> List.map (Seq.map id) 
                 |> List.map List.ofSeq 
                 |> List.map (processLinePart1 []) 
                 |> List.filter (fun x -> x <> 0) 
                 |> List.groupBy id 
                 |> List.map (fun x -> (fst x) * (snd x).Length) 
                 |> List.sum

let solve2 i = i |> List.map (Seq.map id) 
                 |> List.map List.ofSeq 
                 |> List.filter (fun c -> processLinePart1 [] c = 0) 
                 |> List.map (processLinePart2 []) 
                 |> List.sort 
                 |> List.indexed
                 |> (fun l -> (l |> Map.ofList).[l.Length/2])

printfn "Solution 1: %O" (solve1 input)
printfn "Solution 2: %O" (solve2 input)