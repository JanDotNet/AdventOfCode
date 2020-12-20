open System
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day18.txt")
let input = File.ReadAllLines(file) |> Array.toList

type Token =
  | Value of int64
  | Operator of char
  | TokenGroup of Token list
  | TokenGroupGen of Token list

let parseLine line =
  line |> Seq.filter (fun x -> x <> ' ') |> Seq.toList

let parse input =
    let parse' (tokens, stack) c =
      let parseInt s = 
        s |> string |> (Int64.TryParse >> snd)

      let isOp op = 
        ['+'; '-'; '*'] |> List.contains op

      let addToken token =
        match stack with
        | [] -> (token::tokens, [])
        | curGrp::tail -> (tokens, (token::curGrp)::tail)

      match c with
      | '(' -> match stack with 
               | [head] -> (TokenGroup(head) :: tokens, [])
               | head::h2::tail -> tokens, (TokenGroup(head)::h2)::tail
      | ')' -> (tokens, []::stack)
      | op  when  op |> isOp -> Operator(op) |> addToken          
      | num when num |> Char.IsDigit -> Value(num |> parseInt) |> addToken
    
    input |> List.rev |> List.fold parse' ([], []) |> fst
 
let rec precedenceAdd tokens =

  let rec evalToken = function
    | Value(num) -> Value(num)
    | TokenGroup(tokenGroup) -> TokenGroup(eval' tokenGroup)
    | TokenGroupGen(tokenGroup) -> TokenGroupGen(tokenGroup)
    | _ -> failwith "invalid token"

  and eval' = function
    | tl::Operator(op)::tr::t' when op = '+'  -> eval' (TokenGroupGen([(evalToken tl);Operator(op);(evalToken tr)])::t')
    | tl::Operator(op)::tr::t'                -> (tl |> evalToken)::Operator(op)::(eval' (tr::t'))
    | [t]                                     -> [t |> evalToken]
    | []                                      -> []
    | x -> failwithf "invalid case %A" x
  
  tokens |> eval'

let rec eval tokens =
  let calc lv rv = function
    | '+' -> lv + rv
    | '-' -> lv - rv
    | '*' -> lv * rv
    | _ -> failwith "invalid operator"

  let rec evalToken = function
    | Value(num) -> num
    | TokenGroup(tokenGroup) -> eval' 0L tokenGroup
    | TokenGroupGen(tokenGroup) -> eval' 0L tokenGroup
    | _ -> failwith "invalid token"

  and eval' result = function
    | [] -> result
    | [TokenGroup(t')] -> eval' result t'
    | [TokenGroupGen(t')] -> eval' result t'
    | Operator(op)::tr::t' -> eval' (calc result (evalToken tr) op) t' 
    | tl::Operator(op)::tr::t' -> eval' (calc (evalToken tl) (evalToken tr) op) t'
    | x -> failwithf "invalid token stream: %A" x
  
  tokens |> eval' 0L

let calc = (parseLine >> parse >> precedenceAdd >> eval)

let a = input |> List.map calc |> List.sum