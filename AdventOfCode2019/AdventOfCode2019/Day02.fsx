open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day02.txt")

let input = File.ReadAllText(file).Split(',') |> Array.map int

let rec processArrayRec (a:int array) pos =
    match a.[pos] with
        | 1 -> a.[a.[pos + 3]] <- a.[a.[pos + 1]] + a.[a.[pos + 2]]; processArrayRec a (pos + 4)
        | 2 -> a.[a.[pos + 3]] <- a.[a.[pos + 1]] * a.[a.[pos + 2]]; processArrayRec a (pos + 4)
        | 99 -> a.[0]
        | x -> -1

for noun in [0..99] do
    for verb in [0..99] do
        let input' = Array.copy input
        input'.[1] <- noun
        input'.[2] <- verb
        if (processArrayRec input' 0) = 19690720 then 
            printf "Founf result! Noun: %i Verb: %i Result: %i" noun verb (100*noun + verb)

        
List.allPairs [1..5] [1..5]

input