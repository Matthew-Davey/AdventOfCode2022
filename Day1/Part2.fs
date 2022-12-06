module Day1.Part2

let run : string seq -> unit =
    Seq.map (function
        | "" -> fun (xs, acc) -> (acc::xs, 0)
        | x -> fun (xs, acc) -> (xs, acc + int x)
    )
    >> Seq.fold (fun acc fn -> fn acc) ([], 0)
    >> fun (xs, acc) -> List.sortDescending (acc::xs)
    >> List.take 3
    >> List.sum
    >> printfn "%i"
