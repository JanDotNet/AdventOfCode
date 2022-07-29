open System.IO
open System

let file = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day22.txt")

let content = "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682"

let input = content.Split('\n') |> List.ofArray
//let input = File.ReadAllLines(file) |> List.ofArray

module Map =
    let merge m1 m2 = m2 |> Map.fold (fun acc key value -> acc |> Map.add key value) m1
    let addRange range map = range |> Seq.fold (fun acc (key, value) -> acc |> Map.add key value) map

type Cubiod = {On:bool; X1:int; X2:int; Y1:int; Y2:int; Z1:int; Z2:int } with
    static member cut size c = 
        let cutL low = max low -size
        let cutH high = min high -size
        {c with X1 = c.X1 |> cutL; 
                X2 = c.X2 |> cutH; 
                Y1 = c.Y1 |> cutL;
                Y2 = c.Y2 |> cutH;
                Z1 = c.Z1 |> cutL;
                Z2 = c.Z2 |> cutH }

    static member parse (str:string) =
        let splitRange (str:string) =
            let s = str.Split ".."
            s.[0] |> int, s.[1] |> int    
        let s1 = str.Split " x="
        let on = s1.[0] = "on"
        let s2 = s1.[1].Split ",y="
        let x1, x2 = s2.[0] |> splitRange
        let s3 = s2.[1].Split ",z="
        let y1, y2 = s3.[0] |> splitRange
        let z1, z2 = s3.[1] |> splitRange
        {On = on; X1 = x1; X2 = x2; Y1 = y1; Y2 = y2; Z1 = z1; Z2 = z2 }
    
    static member overlap c1 c2 =
        let inline isWithin a1 a2 b1 b2 = (b1 > a2 || b2 < a1) |> not
        isWithin c1.X1 c1.X2 c2.X1 c2.X2 &&
        isWithin c1.Y1 c1.Y2 c2.Y1 c2.Y2 &&
        isWithin c1.Z1 c1.Z2 c2.Z1 c2.Z2
    
    static member intersect c1 c2 =
        let x1 = max c1.X1 c2.X1
        let x2 = min c1.X2 c2.X2
        let y1 = max c1.Y1 c2.Y1
        let y2 = min c1.Y2 c2.Y2
        let z1 = max c1.Z1 c2.Z1
        let z2 = min c1.Z2 c2.Z2
        {On = true; X1 = x1; X2 = x2; Y1 = y1; Y2 = y2; Z1 = z1; Z2 = z2}

    static member volume c = 
        (c.X2 - c.X1) * (c.Y2 - c.Y1) * (c.Z2 - c.Z1)


let folder (processed, totalVolume) c =
    let overlapVolumes volTotal rem c = 
        match rem with
        | h::t -> if Cubiod.overlap h c 
                  then  let c' = Cubiod.intersect h c
                        volTotal + 

    let volume = c |> Cubiod.volume


input |> List.map Cubiod.parse
      |> List.rev
      |> List.fold folder ([], 0)