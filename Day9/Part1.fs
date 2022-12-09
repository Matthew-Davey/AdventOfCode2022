module Day9.Part1

let (|Move|_|) (dir : char) (x : string) = if x[0] = dir then Some (int (x[2..])) else None

let moveTail (hx : int, hy : int) (tx : int, ty : int) =
    printfn "head=%A tail=%A diff=%A" (hx, hy) (tx, ty) (hx - tx, hy - ty)
    match (hx - tx, hy - ty) with
    | (-2, 0) -> (hx + 1, hy)
    | (2, 0) -> (hx - 1, hy)
    | (0, -2) -> (hx, hy + 1)
    | (0, 2) -> (hx, hy - 1)
    | (-1, -2) -> (tx + 1, tx + 1)
    | (-2, -1) -> (tx + 1, tx + 1)
    | (1, 2) -> (tx - 1, tx - 1)
    | (2, 1) -> (tx - 1, tx - 1)
    | _ -> (tx, ty)

let run  (input : string array) =
    input
    |> Seq.collect (fun move -> Seq.replicate (int move[2..]) $"{move[0]} 1")
    |> Seq.fold (fun ((hx, hy), hist) move ->
        let (tx, ty) = List.last hist
        match move with
        | "R 1" -> (hx + 1, hy), hist@[(moveTail (hx + 1, hy) (tx, ty))]
        | "L 1" -> (hx - 1, hy), hist@[(moveTail (hx - 1, hy) (tx, ty))]
        | "U 1" -> (hx, hy + 1), hist@[(moveTail (hx, hy + 1) (tx, ty))]
        | "D 1" -> (hx, hy - 1), hist@[(moveTail (hx, hy - 1) (tx, ty))]
    ) ((0, 0), [(0,0)])
    |> fun (head, hist) ->
        printfn "%A" hist
        (head, hist)
    |> fun (_, hist) -> Set.ofList hist
    |> Set.count
    |> printfn "%i"