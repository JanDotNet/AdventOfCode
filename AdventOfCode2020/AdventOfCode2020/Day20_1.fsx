open System
open System.IO
open System.Text.RegularExpressions

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day20.txt")
let input = File.ReadAllLines(file) |> Array.toList

type Tile = { Id : int; Left : string; Top : string; Right : string; Bottom: string } with
  static member Parse (lines : string list) =
    let id = lines.Head.Substring(5, 4) |> int
    let imageLines = lines |> List.skip 1 |> List.take 10
    let top = imageLines |> List.head 
    let bottom = imageLines |> List.last
    let left = imageLines |> List.map (fun l -> l.[0]) |> String.Concat
    let right = imageLines |> List.map (fun l -> l.[9]) |> String.Concat
    { Id = id; Left = left; Top = top; Right = right; Bottom = bottom }

  static member FlipHorizontal tile = {tile with Left = tile.Left |> Seq.rev |> String.Concat; 
                                                 Right = tile.Right |> Seq.rev |> String.Concat }

  static member FlipVertical tile = {tile with Top = tile.Top |> Seq.rev |> String.Concat; 
                                               Bottom = tile.Bottom |> Seq.rev |> String.Concat }

let tiles = input 
             |> List.chunkBySize 12 
             |> List.map Tile.Parse

let tilesFliped = tiles |> List.map (Tile.FlipHorizontal >> Tile.FlipVertical)

let cornerSites = List.concat [tiles; tilesFliped] 
                |> List.collect (fun t -> [t.Left; t.Right; t.Top; t.Bottom]) 
                |> List.groupBy id
                |> List.filter (fun (_, grp) -> grp |> List.length = 1)
                |> List.map fst
                |> Set.ofList

let corners = tiles 
            |> List.filter (fun t -> (Set.intersect ([t.Left; t.Right; t.Top; t.Bottom] |> Set.ofList) cornerSites).Count = 2)
            |> List.map (fun t -> t.Id |> int64)
            |> List.reduce (*)
