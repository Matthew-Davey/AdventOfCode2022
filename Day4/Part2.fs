module Day4.Part2

module String = let split (separator : char) (x : string) = x.Split separator
module Set = let intersects a b = Set.intersect a b |> Set.count > 0

let parseRange x =
    let xs = String.split '-' x
    Set.ofSeq { (int xs[0])..(int xs[1]) }
let parseLine x =
    let xs = String.split ',' x
    (parseRange xs[0], parseRange xs[1])

let run : string array -> unit =
    Array.map parseLine
    >> Array.filter (fun (a, b) -> Set.intersects a b || Set.intersects b a)
    >> Array.length
    >> printfn "%i"
