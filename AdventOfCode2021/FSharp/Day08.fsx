open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day08.txt")

// let input = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
// edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
// fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
// fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
// aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
// fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
// dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
// bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
// egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
// gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce".Split('\n')

let input = File.ReadAllLines(file)

type EntryRecord = { SignalPattern:(Set<char> list);  Output:(Set<char> list); Resolved:Map<int,Set<char>> } 

let parse (line:string) =
    let lineSplitted = line.Split('|')
    let parsePattern (p:string) = p.Trim().Split(' ') |> Array.toList |> List.map (fun p -> p |> Set.ofSeq)
    let signalPattern = lineSplitted.[0] |> parsePattern
    let output = lineSplitted.[1] |> parsePattern
    { SignalPattern = signalPattern; Output = output; Resolved = Map.empty }

let rec resolve (entry:EntryRecord) num =
    let remainingPatterns = entry.SignalPattern |> List.except (entry.Resolved |> Map.toSeq |> Seq.map snd)
    let resolvePattern = function
        | 1 -> remainingPatterns |> Seq.find (fun x -> x.Count = 2)    
        | 4 -> remainingPatterns |> Seq.find (fun x -> x.Count = 4)            
        | 7 -> remainingPatterns |> Seq.find (fun x -> x.Count = 3)            
        | 8 -> remainingPatterns |> Seq.find (fun x -> x.Count = 7)
        | 6 -> remainingPatterns |> Seq.find (fun x -> x.Count = 6 && (entry.Resolved.[1].IsSubsetOf(x) |> not))
        | 3 -> remainingPatterns |> Seq.find (fun x -> x.Count = 5 && entry.Resolved.[1].IsSubsetOf(x))
        | 5 -> remainingPatterns |> Seq.find (fun x -> x.Count = 5 && x.IsSubsetOf(entry.Resolved.[6]))
        | 9 -> remainingPatterns |> Seq.find (fun x -> x.Count = 6 && entry.Resolved.[5].IsSubsetOf(x) && entry.Resolved.[1].IsSubsetOf(x))
        | 0 -> remainingPatterns |> Seq.find (fun x -> x.Count = 6)
        | 2 -> remainingPatterns |> Seq.find (fun x -> x.Count = 5)
        | _ -> failwith("should not happen")
    let pattern = resolvePattern num
    { entry with Resolved = entry.Resolved.Add (num, pattern)}

let encodeOutput (entry:EntryRecord) =
    let swapMap map = map |> Map.toSeq |> Seq.map (fun (a, b) -> b, a) |> Map.ofSeq
    let reverseResolved = entry.Resolved |> swapMap
    entry.Output |> List.map (fun m -> reverseResolved.[m]) |> List.rev |> List.indexed |> List.map (fun (i, v) -> v * (pown 10 i)) |> List.sum

let solve1 entries =
    let uniqueCounts = [2; 3; 4; 7]
    entries |> List.map (fun e -> e.Output) |> List.concat |> List.filter (fun x -> uniqueCounts |> List.contains x.Count) |> List.length

let solve2 entries =
    let resolveOrder = [1; 4; 7; 8; 6; 3; 5; 9; 0; 2]
    let solveEntry entry = resolveOrder |> List.fold resolve entry |> encodeOutput
    entries |> List.map solveEntry |> List.sum

let parsedInput = input |> Array.map parse |> Array.toList

printfn "Solution 1: %i" (parsedInput |> solve1)
printfn "Solution 2: %i" (parsedInput |> solve2)