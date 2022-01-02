open System
open System.IO

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day19.txt")

let content = "..."
//let input = content.Split('\n') |> Array.toList
let input = File.ReadAllLines(file) |> Array.toList

let rad x = (float x) * Math.PI / 180.

type Key = { Scanner:int; Orientation:int; Beacon:int }
type Vec = { X:int; Y:int; Z:int } with
    static member Zero = {X = 0; Y = 0; Z = 0}    
    static member (+) (v1 : Vec, v2 : Vec) = {X = v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z}
    static member (-) (v1 : Vec, v2 : Vec) = {X = v1.X - v2.X; Y = v1.Y - v2.Y; Z = v1.Z - v2.Z}
    static member manhattenDistance (v1:Vec, v2:Vec) = abs(v2.X-v1.X) + abs(v2.Y-v1.Y) + abs(v2.Z-v1.Z)
    static member rotX a v =
        let a' = a |> rad
        let y' = cos a' * (v.Y |> float) - sin a' * (v.Z |> float) |> round |> int
        let z' = sin a' * (v.Y |> float) + cos a' * (v.Z |> float) |> round |> int
        {X = v.X ; Y = y'; Z = z'}
    static member rotY a v =
        let a' = a |> rad
        let x' = cos a' * (v.X |> float) + sin a' * (v.Z |> float) |> round |> int
        let z' = -sin a' * (v.X |> float) + cos a' * (v.Z |> float) |> round |> int
        {X = x'; Y = v.Y; Z = z'}
    static member rotZ a v =
        let a' = a |> rad
        let x' = cos a' * (v.X|> float) - sin a' * (v.Y |> float) |> round |> int
        let y' = sin a' * (v.X |> float) + cos a' * (v.Y |> float) |> round |> int
        {X = x'; Y = y'; Z = v.Z}
    override this.ToString() =
        sprintf "{%i %i %i}" this.X this.Y this.Z

let parse lines =
    let parseLine (line:string) =
        let s = line.Split(',')
        { X = s.[0] |> int; Y = s.[1] |> int; Z = s.[2] |> int}
    let foldScanner (scanners, scanner) line =
        match line with
        | "" -> let scanner' = scanner |> List.rev
                (scanner'::scanners, [])
        | l when l.StartsWith("---") -> (scanners, scanner)
        | coord -> (scanners, (coord |> parseLine)::scanner)
    let scanners, scanner = lines |> List.fold foldScanner ([], [])
    (scanner |> List.rev)::scanners |> List.rev

let rec combineAll list =
    match list with
    | [] -> []
    | h::t -> let list' = seq {
                    for e in t do
                        yield (h, e) } |> Seq.toList
              let list'' = t|> combineAll
              list' @ list''

let rotateAll points =
    seq {
        yield points
        yield points |> List.map (Vec.rotX 90)
        yield points |> List.map (Vec.rotX 180)
        yield points |> List.map (Vec.rotX 270)
        yield points |> List.map (Vec.rotZ 90)
        yield points |> List.map (Vec.rotZ 90 >> Vec.rotX 90)
        yield points |> List.map (Vec.rotZ 90 >> Vec.rotX 180)
        yield points |> List.map (Vec.rotZ 90 >> Vec.rotX 270)
        yield points |> List.map (Vec.rotZ 180)
        yield points |> List.map (Vec.rotZ 180 >> Vec.rotX 90)
        yield points |> List.map (Vec.rotZ 180 >> Vec.rotX 180)
        yield points |> List.map (Vec.rotZ 180 >> Vec.rotX 270)
        yield points |> List.map (Vec.rotZ 270)
        yield points |> List.map (Vec.rotZ 270 >> Vec.rotX 90)
        yield points |> List.map (Vec.rotZ 270 >> Vec.rotX 180)
        yield points |> List.map (Vec.rotZ 270 >> Vec.rotX 270)
        yield points |> List.map (Vec.rotY 90)
        yield points |> List.map (Vec.rotY 90 >> Vec.rotX 90)
        yield points |> List.map (Vec.rotY 90 >> Vec.rotX 180)
        yield points |> List.map (Vec.rotY 90 >> Vec.rotX 270)
        yield points |> List.map (Vec.rotY 270)
        yield points |> List.map (Vec.rotY 270 >> Vec.rotX 90)
        yield points |> List.map (Vec.rotY 270 >> Vec.rotX 180)
        yield points |> List.map (Vec.rotY 270 >> Vec.rotX 270)
    } |> Seq.toList

let rebase (p_target:Vec) (p_rebase:Vec) (p:Vec) =
    let move_vector = p_target - p_rebase
    p + move_vector

let scanners = input |> parse

let data = seq {
    for scanner_idx, scanner in scanners |> List.indexed do
        for orientation_idx, orientation in scanner |> rotateAll |> List.indexed do
            yield scanner_idx, orientation_idx, 0, Vec.Zero // scanner
            for beacon_idx, beacon in orientation |> List.indexed do
                yield (scanner_idx, orientation_idx, beacon_idx+1, beacon) } |> Seq.toList

let exceptScanner sid d = d |> List.filter (fun (s,o,b,v) -> s <> sid)
let filterScanner sid d = d |> List.filter (fun (s,o,b,v) -> s = sid)
let filterOrientation oid d = d |> List.filter (fun (s,o,b,v) -> o = oid)
let filterScannerAndOrientation sid oid d = d |> List.filter (fun (s,o,b,v) -> s = sid && o = oid)
let mapValue d = d |> List.map (fun (s,o,b,v)-> v)
let mapScanner d = d |> List.map (fun (s,o,b,v)-> s)

data |> filterScanner 1 |> filterOrientation 2
let rec transform data =
    let rec transform' transformed data =
        match data with
        | [] -> transformed
        | _ ->  let scanners_transformed = transformed |> mapScanner |> List.distinct
                let scanners_data = data |> mapScanner |> List.distinct
                printfn "Scanners Transformed: %A Scanners Data: %A" scanners_transformed scanners_data
                let matched = seq { let values_transformed = transformed |> List.filter (fun (s,o,b,v) -> b <> 0) |> mapValue |> Set.ofList                                    
                                    for s_data in scanners_data do                                            
                                        let beacons_d = data |> filterScanner s_data
                                        for orientation in [0..23] do
                                            let s2_data = beacons_d |> filterOrientation orientation
                                            let s2_beacons = s2_data |> List.skip 1 |> mapValue
                                            for s2_beacon in s2_beacons do
                                                for s1_beacon in values_transformed do
                                                    let rebase' = rebase s1_beacon s2_beacon
                                                    let s2_beacons_rebased = s2_beacons |> List.map rebase' |> Set.ofList
                                                    if (values_transformed |> Set.intersect s2_beacons_rebased |> Set.count >= 12) 
                                                    then yield s2_data |> List.map (fun (s, o, b, v) -> (s, o, b, rebase' v))                                                   
                                                    } |> Seq.tryHead
                match matched with
                | Some (m) -> let transformed' = m @ transformed
                              let scanner = m |> mapScanner |> List.head
                              let data' = data |> exceptScanner scanner
                              transform' transformed' data'
                | _ -> failwith "no matching pattern found"
    let firstScanner = data |> mapScanner |> List.min
    let transformed' = data |> filterScannerAndOrientation firstScanner 0
    let data' = data |> exceptScanner firstScanner
    transform' transformed' data'

let transformed = data |> transform

let solve1 transformed = transformed |> List.filter (fun (s, o, b, v) -> b <> 0) |> mapValue |> List.distinct |> List.length
let solve2 transformed = transformed|> List.filter (fun (s, o, b, v) -> b = 0) |> mapValue |> combineAll |> List.map Vec.manhattenDistance |> List.max

printfn "Solution 01: %i" (transformed |> solve1)
printfn "Solution 02: %i" (transformed |> solve2)
