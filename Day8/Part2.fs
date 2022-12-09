module Day8.Part2

module Array2D =
    let toJagged xy = 
        [| for x in [0..Array2D.length1 xy-1] do
            yield [| for y in [0..Array2D.length2 xy-1] -> xy[x, y] |]
        |]
    let mapRows fn = toJagged >> (Seq.map fn)
    let mapr<'a> fn (xy: 'a[,]) = Array2D.mapi (fun x y v -> fn y v xy[x, *]) xy
    let transpose<'a> = toJagged >> Array.transpose >> array2D
    let combine<'a, 'b, 'c> (a : 'a[,]) (b : 'b[,]) (fn : 'a -> 'b -> 'c) = Array2D.mapi (fun x y v -> fn v b[x, y]) a

let loadForest =
    Seq.map (Seq.map int)
    >> Seq.map (Seq.map (fun x -> x - 48))
    >> array2D

let takeWhilePrev fn =
    Seq.fold (fun acc i ->
        match acc with
        | [] -> i::acc
        | xs -> if fn (Seq.head xs) then i::acc else acc
    ) []

let countVisibleTrees i v (xs : int array) =
    match i with
    | 0 ->  0
    | i when i = (Seq.length xs) - 1 -> 0
    | i ->
        let visibleLeft = xs[..i-1] |> Seq.rev |> takeWhilePrev (fun x -> x < v)
        let visibleRight = xs[i+1..] |> takeWhilePrev (fun x -> x < v)
        Seq.length visibleLeft * Seq.length visibleRight

let run (input : string array) =
    let forest = loadForest input
    let h = forest |> Array2D.mapr countVisibleTrees
    let v = forest |> Array2D.transpose |> Array2D.mapr countVisibleTrees |> Array2D.transpose

    Array2D.combine h v (fun a b -> match a, b with 0, 0 -> 0 | a, b -> a * b)
    |> Array2D.mapRows (Seq.max)
    |> Seq.max
    |> printfn "%A"
