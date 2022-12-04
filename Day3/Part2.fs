module Day3.Part2

module Tuple = let map f (a, b, c) = (f a, f b, f c)

let (|LowerCaseLetter|_|) (c : char) = if int c > 96 && int c <= 122 then Some c else None
let (|UpperCaseLetter|_|) (c : char) = if int c > 64 && int c <= 90 then Some c else None

let (^) a b = Set.intersect a b

let priority = function
| LowerCaseLetter c -> int c - 96
| UpperCaseLetter c -> int c - 38

let run : string array -> unit =
    Array.chunkBySize 3
    >> Array.map (fun x -> (x[0], x[1], x[2]))
    >> Array.map (Tuple.map Set)
    >> Array.map (fun (a, b, c) -> a ^ b ^ c)
    >> Array.map (Set.map priority)
    >> Array.map (Seq.sum)
    >> Array.sum
    >> printfn "%i"
