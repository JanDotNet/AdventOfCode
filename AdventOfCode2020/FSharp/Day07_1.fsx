open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day07.txt")

let bagDefinitions = File.ReadAllLines(file) |> List.ofArray;

type Bag = { Type:string; Color:string; Content:BagContent }
and BagContent = 
    | NotAvaiable
    | NoContent
    | Bags of (int * Bag) list

(* Parser *)
let parseBag (line:string) (bags:Bag list) =
    let words = line.Split(' ') |> List.ofArray

    let findBag t c bag = bag.Type = t && bag.Color = c 

    let rec parseContentBags = function  
    | n :: t :: c :: _ :: tail -> 
        let bag = bags |> List.tryFind (findBag t c)
        let otherBags = parseContentBags tail
        match bag, otherBags with
        | Some b, Some bs -> Some ((n |> int, b) :: bs)
        | _ -> None
    | [] -> Some([])
    | x -> failwith("Invalid case")

    let rec parseContent = function
    | "no" :: "other" :: "bags." :: [] -> NoContent    
    | lst ->
        match parseContentBags lst with
        | Some bags -> Bags(bags)
        | None -> NotAvaiable

    match words with
    | t :: c :: "bags" :: "contain" :: tail -> { Type = t; Color = c; Content = parseContent tail }
    | _ -> failwith "invalid input"

let rec foldBagsToTree (bags, nonParsedLines) line =
    match parseBag line bags with
    | { Type = t; Color = c; Content = NotAvaiable} -> (bags, line :: nonParsedLines)
    | bag -> (bag :: bags, nonParsedLines)

let rec processIterative = function 
    | (bags, []) -> (bags, [])
    | (bags, lines) -> lines |> List.fold foldBagsToTree (bags, []) |> processIterative

let bagsTree = ([], bagDefinitions) |> processIterative |> fst

(* processing *)
let isShinyGoldBag bag = bag.Type = "shiny" && bag.Color = "gold"

let rec CollectTreeAboveShinyGold bags bag =
    if (bag |> isShinyGoldBag)
    then bags
    else
        match bag.Content with
        | NotAvaiable -> failwith "should not happen"
        | NoContent -> printfn "found end"; []
        | Bags (bagsList) -> bagsList |> List.map snd |> List.fold CollectTreeAboveShinyGold (bag :: bags) 
 
let rec CollectBagsAboveShinyGold bag =
    if (bag |> isShinyGoldBag)
    then [bag]
    else match bag.Content with
    | NotAvaiable -> failwith "should not happen"
    | NoContent -> []
    | Bags (bagsList) ->
        let children = bagsList |> List.map snd |> List.map CollectBagsAboveShinyGold |> List.concat
        if (children |> List.length > 0) then bag :: children else []


let result = bagsTree 
                  |> List.map CollectBagsAboveShinyGold
                  |> List.concat
                  |> List.filter (isShinyGoldBag >> not)
                  |> List.map (fun b -> (b.Color, b.Type)) 
                  |> List.distinct
                  |> List.length