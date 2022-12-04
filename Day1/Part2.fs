module Day1.Part2

// Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?
let run : string array -> unit =
    Array.fold (fun accumulator item ->
        match item with
        | "" -> []::accumulator
        | _ -> match accumulator with
               | [] -> [[item]]
               | head::tail -> (item::head)::tail
    ) []
    >> List.map (List.map int)
    >> List.map List.sum
    >> List.sortDescending
    >> List.take 3
    >> List.sum
    >> printfn "%i"
