module Day4.Part1

module String = let split (separator : char) (x : string) = x.Split separator

let parseRange = String.split '-' >> fun xs -> Set.ofSeq { (int xs[0])..(int xs[1]) }
let parseLine = String.split ',' >> fun xs -> parseRange xs[0], parseRange xs[1]

let run : string array -> unit =
    Array.map parseLine
    >> Array.filter (fun (a, b) -> Set.isSubset a b || Set.isSubset b a)
    >> Array.length
    >> printfn "%i"
