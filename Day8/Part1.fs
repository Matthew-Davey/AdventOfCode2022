module Day8.Part1

module Array2D =
    let toJagged xy = 
        [| for x in [0..Array2D.length1 xy-1] do
            yield [| for y in [0..Array2D.length2 xy-1] -> xy[x, y] |]
        |]
    let mapr<'a> fn (xy: 'a[,]) =
        Array2D.mapi (fun x y v -> fn y xy[x, *]) xy
    let transpose<'a> = toJagged >> Array.transpose >> array2D
    let zip<'a, 'b, 'c> (a : 'a[,]) (b : 'b[,]) (fn : 'a -> 'b -> 'c) =
        Array2D.mapi (fun x y v -> fn v b[x, y]) a

let loadForest =
    Seq.map (Seq.map int)
    >> Seq.map (Seq.map (fun x -> x - 48))
    >> array2D

let visibleFromOutside i (xs : int array) =
    match i with
    | 0 -> 1
    | i when i = Seq.length xs - 1 -> 1
    | i -> if xs[i] > Seq.max xs[..i-1] || xs[i] > Seq.max xs[i+1..] then 1 else 0

let run (input : string array) =
    let forest = loadForest input
    let h = forest |> Array2D.mapr visibleFromOutside
    let v = forest |> Array2D.transpose |> Array2D.mapr visibleFromOutside |> Array2D.transpose

    Array2D.zip h v max
    |> Array2D.toJagged
    |> Seq.map (Seq.sum)
    |> Seq.sum
    |> printfn "%A"
