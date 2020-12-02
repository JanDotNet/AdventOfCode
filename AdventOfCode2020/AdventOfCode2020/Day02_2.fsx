open System.IO
open System
open System.Text.RegularExpressions

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day02.txt")

type Entry = { Pos1:int; Pos2:int; Char:char; Password:string } with

    static member parse (line:string) =
        let pattern = "(?<p1>\d+)-(?<p2>\d+)\s(?<char>[a-z]):\s(?<pw>[a-z]+)"
        let m = Regex.Match(line, pattern)
        match m.Success with
        | true -> Some { Password = m.Groups.["pw"].Value;
                         Pos1 = m.Groups.["p1"].Value |> int
                         Pos2 = m.Groups.["p2"].Value |> int
                         Char = m.Groups.["char"].Value |> char }
        | _ -> None

    static member validate (entry:Entry) =
        let getCharAtPos pos = if entry.Password.Length >= pos 
                               then entry.Password.[pos-1]
                               else ' '
        let pos1 = getCharAtPos entry.Pos1
        let pos2 = getCharAtPos entry.Pos2
        (entry.Char = pos1) <> (entry.Char = pos2)

module Seq = 
    let filterSomeUnbox list = list |> Seq.filter Option.isSome |> Seq.map Option.get

let result = File.ReadAllLines(file)
            |> Seq.map Entry.parse
            |> Seq.filterSomeUnbox
            |> Seq.filter Entry.validate
            |> Seq.length
    