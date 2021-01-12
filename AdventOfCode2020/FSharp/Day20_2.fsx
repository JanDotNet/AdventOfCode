open System
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day20.txt")
let input = File.ReadAllLines(file) |> Array.toList
let monster = [(0, 18);(1,0);(1,5);(1,6);(1,11);(1,12);(1,17);(1,18);(1,19);(2,1);(2,4);(2,7);(2,10);(2,13);(2,16)]

let flip (s:string) = s |> (Seq.rev >> String.Concat)
let hasOneOf l1 l2 = l1 |> List.exists (fun i1 -> l2 |> List.exists (fun i2 -> i1 = i2))

type Tile = { Id : int; Content: char list list;
              Left : string;
              Top : string;
              Right : string;
              Bottom: string; } with
  member this.LeftRev with get () = this.Left |> flip
  member this.RightRev with get () = this.Right |> flip
  member this.TopRev with get ()= this.Top |> flip
  member this.BottomRev with get ()= this.Bottom |> flip
  member this.AllTop with get () = [this.Top; this.TopRev]
  member this.AllBottom with get () = [this.Bottom; this.BottomRev]
  member this.AllLeft with get () = [this.Left; this.LeftRev]
  member this.AllRight with get () = [this.Right; this.RightRev]
  member this.AllSites with get () = [this.Left; this.LeftRev; this.Right; this.RightRev; this.Top; this.TopRev; this.Bottom; this.BottomRev]
  

  static member HasAnyLeft sides (tile:Tile) = sides |> hasOneOf tile.AllLeft
  static member HasAnyTop sides (tile:Tile) = sides |> hasOneOf tile.AllTop
  static member HasAnyRight sides (tile:Tile) = sides |> hasOneOf tile.AllRight
  static member HasAnyBottom sides (tile:Tile) = sides |> hasOneOf tile.AllBottom

  static member Empty = { Id = 0; Content = []; Top = ""; Left = ""; Right = ""; Bottom = "" }

  static member Parse (lines : string list) =
    let id = lines.Head.Substring(5, 4) |> int
    let imageLines = lines |> List.skip 1 |> List.take 10
    let top = imageLines |> List.head 
    let bottom = imageLines |> List.last
    let left = imageLines |> List.map (fun l -> l.[0]) |> String.Concat
    let right = imageLines |> List.map (fun l -> l.[9]) |> String.Concat
    { Id = id; Content = imageLines |> (List.skip 1 >> List.take 8 >> List.map (Seq.skip 1 >> Seq.take 8 >> Seq.toList))
      Left = left;
      Top = top;
      Right = right;
      Bottom = bottom }

  static member TryForLeft sides tile =
    if    tile |> Tile.HasAnyLeft sides   then Some(tile)
    elif  tile |> Tile.HasAnyRight sides  then Some(tile |> Tile.FlipVertical)
    elif  tile |> Tile.HasAnyTop sides    then Some(tile |> Tile.RotateLeft)
    elif  tile |> Tile.HasAnyBottom sides then Some(tile |> Tile.RotateRight)
    else  None

  static member TryForTop sides tile =
    if    tile |> Tile.HasAnyTop sides    then Some(tile)
    elif  tile |> Tile.HasAnyBottom sides then Some(tile |> Tile.FlipHorizontal)
    elif  tile |> Tile.HasAnyRight sides  then Some(tile |> Tile.RotateLeft)
    elif  tile |> Tile.HasAnyLeft sides   then Some(tile |> Tile.RotateRight)
    else  None

  static member TryGet topSides leftSides tile =
    tile |> Tile.TryForLeft leftSides |> Option.bind (Tile.TryForTop topSides)

  static member FlipVertical tile = {tile with Left = tile.Right; 
                                               Right = tile.Left;
                                               Top = tile.TopRev
                                               Bottom = tile.BottomRev;
                                               Content = tile.Content |> List.map List.rev }

  static member FlipHorizontal tile = {tile with Top = tile.Bottom; 
                                                 Bottom = tile.Top;
                                                 Left = tile.LeftRev;
                                                 Right = tile.RightRev;
                                                 Content = tile.Content |> List.rev }

  static member RotateRight tile = {tile with Top = tile.Left;
                                              Right = tile.Top;
                                              Bottom = tile.Right;
                                              Left = tile.Bottom;
                                              Content = tile.Content |> List.transpose |> List.map List.rev }

  static member RotateLeft tile = {tile with Top = tile.Right;
                                             Right = tile.Bottom;
                                             Bottom = tile.Left;
                                             Left = tile.Top;
                                             Content = tile.Content |> List.transpose |> List.rev }

type Monster (positions:(int*int)list) = 
  static let rotateLeft (px, py) = (py, -px)
  static let translate x y (px, py) = (px + x, py + y)
  static let translateX x = translate x 0
  static let translateY y = translate 0 y
  static let flipHorizontal (px, py) = (px, -py)
  static let flipVertical (px, py) = (-px, py)
  static let width m = (m |> List.map fst |> List.max)
  static let height m = (m |> List.map snd |> List.max)
  with 
  static member RotateLeft monster = monster |> List.map (rotateLeft >> translateY (monster |> width))
  static member FlipHorizontal monster = monster |> List.map (flipHorizontal >> translateY (monster |> height))
  static member FlipVertical monster = monster |> List.map (flipVertical >> translateX (monster |> width))
  static member AllOrientations monster = seq {
      yield monster
      yield monster |> (Monster.RotateLeft)
      yield monster |> (Monster.RotateLeft >> Monster.RotateLeft)
      yield monster |> (Monster.RotateLeft >> Monster.RotateLeft >> Monster.RotateLeft)
      yield monster |> (Monster.FlipHorizontal)
      yield monster |> (Monster.FlipHorizontal >> Monster.RotateLeft)
      yield monster |> (Monster.FlipHorizontal >> Monster.RotateLeft >> Monster.RotateLeft)
      yield monster |> (Monster.FlipVertical)
      yield monster |> (Monster.FlipVertical >> Monster.RotateLeft)
      yield monster |> (Monster.FlipVertical >> Monster.RotateLeft >> Monster.RotateLeft)
    }

let allTiles = input 
             |> List.chunkBySize 12 
             |> List.map Tile.Parse

let allSites = allTiles 
               |> List.collect (fun t -> t.AllSites)

let edgeSites = allSites
                |> List.groupBy id
                |> List.filter (fun (_, grp) -> grp |> List.length = 1)
                |> List.map fst
                |> Set.ofList

let cornerSites = allTiles 
                |> List.filter (fun t -> (Set.intersect (t.AllSites |> Set.ofList) edgeSites).Count = 4)
                |> List.collect (fun t -> t.AllSites)
                |> List.filter (fun s -> edgeSites |> Set.contains s)

let getTile t l tiles = tiles 
                        |> List.map (Tile.TryGet t l) 
                        |> List.filter Option.isSome 
                        |> List.head 
                        |> Option.get

let seaTiles = Array2D.create 12 12 Tile.Empty

let mutable processed = []
for row in [0..(seaTiles |> Array2D.length2)-1] do
  for col in [0..(seaTiles |> Array2D.length1)-1] do
    let top = match (row, col) with
              | (0, 0) -> cornerSites
              | (0, _) -> edgeSites |> Set.toList
              | _      -> seaTiles.[row-1, col].AllBottom
    let left = match (row, col) with
               | (0, 0) -> cornerSites
               | (_, 0) -> edgeSites |> Set.toList
               | _      -> seaTiles.[row, col-1].AllRight
    let remaining = allTiles |> List.filter (fun t -> processed |> List.contains t.Id |> not)
    let t =  remaining |> getTile top left
    processed <- t.Id::processed
    seaTiles.[row, col] <- t

let createPicture (array:Tile[,]) = seq {
  for row in [0..(array |> Array2D.length2)-1] do
    let tilesInRow = array.[row,*]
    for rowInTile in [0..7] do 
      yield tilesInRow |> Array.collect (fun t -> t.Content.[rowInTile] |> List.toArray) }

let seaField = seaTiles |> createPicture |> Seq.toArray

let countSeaMonsters (sea:char [][]) monster =
  let seaWidth = sea.Length
  let seaHeight = sea.[0].Length
  let monsterWidth = (monster |> List.map snd |> List.max) + 1
  let monsterHeight = (monster |> List.map fst |> List.max) + 1
  let lastCol = seaWidth - monsterWidth
  let lastRow = seaHeight - monsterHeight

  let rec scan row col count =
    if row = lastRow then count
    else
      let nextRow = if col = lastCol then row + 1 else row
      let nextCol = if col = lastCol then 0 else col + 1
      let foundMonster = monster |> List.forall (fun (r, c) -> sea.[row + r].[col + c] = '#')
      let nextCount = if foundMonster then count + 1 else count
      scan nextRow nextCol nextCount
    
  scan 0 0 0  

let swap (x, y) = (y, x)

let total = seaField |> Array.collect id |> Array.filter (fun x -> x = '#') |> Array.length
let count = monster |> List.map swap 
                    |> Monster.AllOrientations 
                    |> Seq.toList 
                    |> Seq.map (List.map swap) 
                    |> Seq.toList 
                    |> Seq.map (countSeaMonsters seaField) 
                    |> Seq.filter (fun count -> count <> 0)
                    |> Seq.head

let result = total - (count * 15)