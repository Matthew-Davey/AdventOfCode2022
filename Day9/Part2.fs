module Day9.Part2

let moveKnot (hx, hy) (tx, ty) =
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
    | (2, 2) -> (hx + 1, hy + 1)
    | (2, -2) -> (hx + 1, hy - 1)
    | (-2, 2) -> (hx - 1, hy + 1)
    | (-2, -2) -> (hx - 1, hy - 1)
    | _ -> (tx, ty)

let run : string array -> unit =
    Seq.collect (fun move -> Seq.replicate (int move[2..]) $"{move[0]} 1")
    >> Seq.map (function
    | "R 1" -> fun (hx, hy) -> (hx + 1, hy)
    | "L 1" -> fun (hx, hy) -> (hx - 1, hy)
    | "U 1" -> fun (hx, hy) -> (hx, hy + 1)
    | "D 1" -> fun (hx, hy) -> (hx, hy - 1)
    )
    >> Seq.fold (fun (knots, hist) move ->
        List.fold (fun (i, fn, knots) knot ->
            let knot = fn knot
            (i + 1, moveKnot knot, knots@[knot])
        ) (0, move, []) knots
        |> fun (_, _, knots) -> (knots, List.last knots::hist)
    ) (List.replicate 10 (0, 0), [])
    >> fun (_, hist) -> Set.ofList hist
    >> Set.count
    >> printfn "%i"
