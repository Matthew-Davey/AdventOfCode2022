module Day1.Part1

// Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
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
    >> List.max
    >> printfn "%i"
