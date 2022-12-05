module Day1.Part1

let run : string array -> unit =
    Array.map (function
        | "" -> fun (xs, acc) -> (acc::xs, 0)
        | x -> fun (xs, acc) -> (xs, acc + int x)
    )
    >> Array.fold (fun acc fn -> fn acc) ([], 0)
    >> fun (xs, acc) -> List.max (acc::xs)
    >> printfn "%i"
