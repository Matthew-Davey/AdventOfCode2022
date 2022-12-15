module Day14.Part1

module String = let split (separator : string) (s : string) = s.Split(separator)

let parseLine line =
    String.split " -> " line
    |> Seq.map (String.split "," >> fun [|x; y|] -> (int x, int y))
    |> Seq.pairwise
    |> Seq.map (function
        | (x0, y0), (x1, y1) when x1 < x0 -> (x1, y1), (x0, y0)
        | (x0, y0), (x1, y1) when y1 < y0 -> (x1, y1), (x0, y0)
        | (x0, y0), (x1, y1) -> (x0, y0), (x1, y1))

let findExtents = Seq.fold (fun (mx, my) (_, (x, y)) -> (max mx x, max my y)) (0, 0)

let parseTable input =
    let lines = Seq.collect parseLine input
    let mx, my = findExtents lines
    let table = Array2D.create (mx + 1) (my + 1) 0
    Seq.iter (fun ((x0, y0), (x1, y1)) ->
        for x in x0..x1 do
        for y in y0..y1 do
            table[x, y] <- 1
    ) lines
    table

let rec descend table (x, y) =
    if y = Array2D.length2 table - 1 then None else
    match (table[x - 1, y + 1], table[x, y + 1], table[x + 1, y + 1]) with
    | (_, 0, _) -> descend table (x, y + 1)
    | (0, 1, _) -> descend table (x - 1, y + 1)
    | (1, 1, 0) -> descend table (x + 1, y + 1)
    | (1, 1, 1) -> Some (x, y)

let rec iterate i table =
    match descend table (500, 0) with
    | Some (x, y) -> table[x, y] <- 1; iterate (i + 1) table
    | None -> i - 1

let run : string array -> unit = parseTable >> iterate 1 >> printfn "%d"
