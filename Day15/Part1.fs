module Day15.Part1

module String = let split (separator : string) (s : string) = s.Split separator

type Sensor = { Position : int32 * int32; Beacon : int32 * int32; Radius : int32 }

let manhattan (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)

let parseSensor line =
    let [| sensor; beacon |] = String.split ": " line
    let [| sx; sy |] = String.split ", " sensor
    let [| bx; by |] = String.split ", " beacon
    let sx, sy = (int32 sx[12..], int32 sy[2..])
    let bx, by = (int32 bx[23..], int32 by[2..])
    { Position = sx, sy; Beacon = bx, by; Radius = manhattan (sx, sy) (bx, by) }

let run input row =
    let sensors = Seq.map parseSensor input
    let beacons =
        Seq.filter (fun sensor -> snd sensor.Beacon = row) sensors
        |> Seq.map (fun sensor -> fst sensor.Beacon)
    
    Seq.collect(fun sensor ->
        let x, y = sensor.Position
        let margin = (sensor.Radius - abs (row - y)) in [x - margin..x + margin]
    ) sensors
    |> Seq.except beacons
    |> (Seq.distinct >> Seq.length)
    |> printfn "%d"
