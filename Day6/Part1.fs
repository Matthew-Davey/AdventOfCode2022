module Day6.Part1

let run : string -> unit =
    Seq.windowed 4
    >> Seq.indexed
    >> Seq.map (fun (i, xs) -> (i, Set.ofArray xs))
    >> Seq.find (fun (i, xs) -> Seq.length xs = 4)
    >> fun (i, _) -> i + 4
    >> printfn "%i"
