open System.IO

// Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
File.ReadAllLines("./input.txt")
|> Array.fold (fun accumulator item ->
    match item with
    | "" -> []::accumulator
    | _ -> match accumulator with
           | [] -> [[item]]
           | head::tail -> (item::head)::tail
) []
|> List.map (List.map int)
|> List.map List.sum
|> List.max
|> printfn "%i"

// Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?
File.ReadAllLines("./input.txt")
|> Array.fold (fun accumulator item ->
    match item with
    | "" -> []::accumulator
    | _ -> match accumulator with
           | [] -> [[item]]
           | head::tail -> (item::head)::tail
) []
|> List.map (List.map int)
|> List.map List.sum
|> List.sortDescending
|> List.take 3
|> List.sum
|> printfn "%i"
