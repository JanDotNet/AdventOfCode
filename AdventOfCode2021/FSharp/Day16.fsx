open System
open System.IO
open System.Collections.Generic

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day16.txt")

let input = File.ReadAllLines(file) |> Array.head

let parse (input:string) = 
    let input' = if input.Length % 2 <> 0 then input + "0" else input
    let binStrings = Convert.FromHexString(input') |> Array.map (fun b -> Convert.ToString(b, 2).PadLeft(8, '0'))
    let binString = String.Join("", binStrings)
    binString |> Seq.map (fun b -> b |> string |> int64) |> Seq.toList

let consumeCount count stream =
    let consumed = stream |> List.take count
    let remaining = stream |> List.skip count
    consumed, remaining

let withRem f (consumed, rem) = 
    consumed |> f, rem

let toInt stream = stream |> List.rev 
                          |> List.indexed 
                          |> List.map (fun (i, b) -> pown 2L i * b) 
                          |> List.sum

type TypeId = 
     | LiteralValue 
     | Operator of int64 
    with static member fromInt = function
                                 | 4L -> LiteralValue
                                 | x -> Operator(x)

type Package = {Version:int64; Type:TypeId; Body:Body}
and Body = 
    | Literal of int64
    | Packages of Package list

let toString stream = 
    let bitStrings = stream |> List.map snd |> List.map string |> List.toArray
    String.Join("", bitStrings)

let parseLiteralBody stream =
    let rec parseLiteral current stream =
        let content, stream' = stream |> consumeCount 5
        match content with
        | 1L::t -> stream' |> parseLiteral (current @ t)
        | 0L::t -> (current @ t), stream'
        | _ -> failwith "Should not happen"
    let bits, rem = stream |> parseLiteral []
    bits |> toInt, rem

let rec parseSubPackagesBody stream =
    let lengthTypeId, stream' = stream |> consumeCount 1 |> withRem toInt
    match lengthTypeId with
    | 0L -> let streamLength, stream'' = stream' |> consumeCount 15 |> withRem toInt
            let subPackagesStream, stream''' = stream'' |> consumeCount (streamLength |> int)
            subPackagesStream |> parseAllPackages, stream'''
    | 1L -> let packagesCount, stream'' = stream' |> consumeCount 11 |> withRem toInt
            stream'' |> parsePackagesCount packagesCount
    | _  -> failwith "shoudl not happen"

and parseAllPackages stream =
    let rec parseAllPackages' packages stream =
        let package, stream' = stream |> parsePackage
        if stream' |> List.isEmpty || (stream' |> List.forall (fun x -> x = 0L))
        then package::packages |> List.rev
        else stream' |> parseAllPackages' (package::packages)
    stream |> parseAllPackages' []

and parsePackagesCount count stream =
    let rec parsePackagesCount' count packages stream =
        if count = 0L
        then packages |> List.rev, stream
        else let package, stream' = stream |> parsePackage
             stream' |> parsePackagesCount' (count-1L) (package::packages)
    stream |> parsePackagesCount' count []

and parsePackage stream =
    let version, stream' = stream |> consumeCount 3 |> withRem toInt
    let typeId, stream'' = stream' |> consumeCount 3 |> withRem (toInt >> TypeId.fromInt)
    match typeId with
    | LiteralValue -> let literal, stream''' = stream'' |> parseLiteralBody
                      {Version=version; Type=typeId; Body=Literal(literal)}, stream'''
    | Operator(_) ->  let packages, stream''' = stream'' |> parseSubPackagesBody
                      {Version=version; Type=typeId; Body=Packages(packages)}, stream'''

let deepSearch packages =
    let rec deepSearch' packagesToSearch packagesFound =
        match packagesToSearch with
        | [] -> packagesFound
        | h::t -> match h.Body with
                  | Literal (x) -> deepSearch' t (h::packagesFound)
                  | Packages (lst) -> deepSearch' (t @ lst) (h::packagesFound)
    deepSearch' packages []

let rec getValue package =
    match package.Type, package.Body with
    | LiteralValue, Literal x -> x
    | Operator x, Packages(packages) -> match x with
                                        | 0L -> packages |> List.map getValue |> List.sum
                                        | 1L -> packages |> List.map getValue |> List.fold (*) 1
                                        | 2L -> packages |> List.map getValue |> List.min
                                        | 3L -> packages |> List.map getValue |> List.max
                                        | 5L -> let v1 = packages |> List.head |> getValue
                                                let v2 = packages |> List.skip 1 |> List.head |> getValue
                                                if v1 > v2 then 1 else 0
                                        | 6L -> let v1 = packages |> List.head |> getValue
                                                let v2 = packages |> List.skip 1 |> List.head |> getValue
                                                if v1 < v2 then 1 else 0
                                        | 7L -> let v1 = packages |> List.head |> getValue
                                                let v2 = packages |> List.skip 1 |> List.head |> getValue
                                                if v1 = v2 then 1 else 0
                                        | _ -> failwith "invalid case"
    | _ -> failwith "invalid case"


let solve1 input = input 
                   |> parse                
                   |> parseAllPackages 
                   |> deepSearch 
                   |> List.map (fun p -> p.Version) 
                   |> List.sum

let solve2 input = input 
                   |> parse                
                   |> parseAllPackages 
                   |> List.head 
                   |> getValue

printfn "Solution 1: %i" (input |> solve1)
printfn "Solution 2: %i" (input |> solve2)

