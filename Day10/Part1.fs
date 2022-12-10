module Day10.Part1

let (|Addx|_|) (s : string) = if s[..3] = "addx" then Some (int s[5..]) else None

let exec acc = function
| "noop" -> acc @ [Seq.last acc]
| Addx n -> let x = Seq.last acc in acc @ [x] @ [x + n]

let run : string array -> unit =
    Seq.fold exec [1]
    >> Seq.indexed
    >> Seq.filter (fun (i, _) -> (i + 21) % 40 = 0)
    >> Seq.map (fun (i, x) -> (i + 1) * x)
    >> Seq.sum
    >> printfn "%i"
