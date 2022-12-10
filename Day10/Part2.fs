module Day10.Part2

let (|Addx|_|) (s : string) = if s[..3] = "addx" then Some (int s[5..]) else None

let exec acc = function
| "noop" -> acc @ [Seq.last acc]
| Addx n -> let x = Seq.last acc in acc @ [x; x + n]

let renderScanline =
    Seq.indexed
    >> Seq.fold (fun s (i, x) -> s + if abs (i - x) < 2 then "#" else " ") ""
    >> printfn "%s"

let run : string array -> unit =
    Seq.fold exec [1]
    >> Seq.chunkBySize 40
    >> Seq.iter renderScanline
