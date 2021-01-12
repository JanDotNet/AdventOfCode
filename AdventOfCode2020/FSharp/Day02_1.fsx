open System.IO
open System
open System.Text.RegularExpressions

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day02.txt")

type Entry = { Min:int; Max:int; Char:char; Password:string }

module Entry =
    let parse (line:string) =
        let pattern = "(?<min>\d+)-(?<max>\d+)\s(?<char>[a-z]):\s(?<pw>[a-z]+)"
        let m = Regex.Match(line, pattern)
        match m.Success with
        | true -> Some { Password   = m.Groups.["pw"].Value;
                         Min        = m.Groups.["min"].Value |> int
                         Max        = m.Groups.["max"].Value |> int
                         Char       = m.Groups.["char"].Value |> char }
        | _ -> None

    let validate (entry:Entry) =
        let count = entry.Password 
                    |> Seq.filter (fun c -> c = entry.Char) 
                    |> Seq.length                    
        count >= entry.Min && count <= entry.Max

let result = File.ReadAllLines(file)
            |> Seq.map Entry.parse
            |> Seq.choose id
            |> Seq.filter Entry.validate
            |> Seq.length
    