module Day9.Part1

let moveTail (hx : int, hy : int) (tx : int, ty : int) =
    match (tx - hx, ty - hy) with
    | (0, 2) -> (hx, hy + 1)
    | (1, 2) -> (hx, hy + 1)
    | (2, 1) -> (hx + 1, hy)
    | (2, 0) -> (hx + 1, hy)
    | (2, -1) -> (hx + 1, hy)
    | (1, -2) -> (hx, hy - 1)
    | (0, -2) -> (hx, hy - 1)
    | (-1, -2) -> (hx, hy - 1)
    | (-2, -1) -> (hx - 1, hy)
    | (-2, 0) -> (hx - 1, hy)
    | (-2, 1) -> (hx - 1, hy)
    | (-1, 2) -> (hx, hy + 1)
    | _ -> (tx, ty)

let run  (input : string array) =
    printfn "Expect 6011"
    input
    |> Seq.collect (fun move -> Seq.replicate (int move[2..]) $"{move[0]} 1")
    |> Seq.fold (fun ((hx, hy: int), hist) move ->
        let (tx, ty) = List.head hist
        match move with
        | "R 1" -> (hx + 1, hy), (moveTail (hx + 1, hy) (tx, ty))::hist
        | "L 1" -> (hx - 1, hy), (moveTail (hx - 1, hy) (tx, ty))::hist
        | "U 1" -> (hx, hy + 1), (moveTail (hx, hy + 1) (tx, ty))::hist
        | "D 1" -> (hx, hy - 1), (moveTail (hx, hy - 1) (tx, ty))::hist
    ) ((0, 0), [(0,0)])
    |> fun (_, hist) -> Set.ofList hist
    |> Set.count
    |> printfn "%i"
