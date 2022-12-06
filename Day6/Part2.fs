module Day6.Part2

let run : string -> unit =
    Seq.windowed 14
    >> Seq.indexed
    >> Seq.map (fun (i, xs) -> (i, Set.ofArray xs))
    >> Seq.find (fun (i, xs) -> Seq.length xs = 14)
    >> fun (i, _) -> i + 14
    >> printfn "%i"
