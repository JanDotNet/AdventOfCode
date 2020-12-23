open System
open System.IO
open System.Text.RegularExpressions

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day19.txt")
let input = File.ReadAllLines(file) |> Array.toList

type RuleContent =
  | Symbol of string
  | IncludeRules of int list
  | IncludeRulesOr of int list * int list

type Rule = { Id:int; Type:RuleContent }
  with static member Parse line = let symbol = Regex.Match(line, "(?<id>\d*): \"(?<char>[a-z])\"")
                                  let includeRules = Regex.Match(line, "(?<id>\d*): (?<ruleIds>(\d*\s?)*)")
                                  let includeRulesOr = Regex.Match(line, "(?<id>\d*): (?<ruleIds1>(\d*\s?)*) \| (?<ruleIds2>(\d*\s?)*)")
                                  let toRules (s:string) = s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray |> List.map int
                                  if symbol.Success then 
                                    { Id = symbol.Groups.["id"].Value |> int; 
                                      Type = Symbol(symbol.Groups.["char"].Value) }
                                  else if includeRulesOr.Success then
                                    { Id = includeRulesOr.Groups.["id"].Value |> int; 
                                      Type = IncludeRulesOr(includeRulesOr.Groups.["ruleIds1"].Value |> toRules, includeRulesOr.Groups.["ruleIds2"].Value |> toRules) }
                                  else if includeRules.Success then
                                    { Id = includeRules.Groups.["id"].Value |> int; 
                                      Type = IncludeRules(includeRules.Groups.["ruleIds"].Value |> toRules) }
                                  else
                                    failwithf "line invalid: %s" line

let ruleLines = input |> List.takeWhile (fun l -> l <> "")
let messages = input |> List.skipWhile (fun l -> l <> "") |> List.skip 1

let rules = ruleLines |> List.map Rule.Parse
let ruleMap = rules |> List.map (fun r -> (r.Id, r.Type)) |> Map.ofList

let toRegex (rMap: Map<int, RuleContent>) (rule: Rule) =
  
  let rec ruleIdsToRules (ruleIds: int list) =
    let toRule rid = rMap |> Map.find rid
    ruleIds |> List.map toRule |> List.map toRegex' |> List.fold (+) ""
  
  and toRegex' = function
    | Symbol(c) -> c
    | IncludeRules (ruleIds) -> ruleIds |> ruleIdsToRules
    | IncludeRulesOr (ruleIdsLeft , ruleIdsRight) -> sprintf "(%s|%s)" (ruleIdsLeft |> ruleIdsToRules) (ruleIdsRight |> ruleIdsToRules)

  Regex(sprintf @"^%s$" (toRegex' rule.Type), RegexOptions.Compiled)

let regex = rules |> List.find (fun r -> r.Id = 0) |> toRegex ruleMap
messages |> List.filter (fun msg -> regex.IsMatch(msg)) |> List.length