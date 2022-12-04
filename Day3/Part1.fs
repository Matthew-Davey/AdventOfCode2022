module Day3.Part1

module Tuple = let map f (a, b) = (f a, f b)

let (|LowerCaseLetter|_|) (c : char) = if int c > 96 && int c <= 122 then Some c else None
let (|UpperCaseLetter|_|) (c : char) = if int c > 64 && int c <= 90 then Some c else None

let (^) a b = Set.intersect a b

let priority = function
| LowerCaseLetter c -> int c - 96
| UpperCaseLetter c -> int c - 38

let run : (string array -> unit) =
    Array.map (Array.ofSeq)
    >> Array.map (function x -> Array.splitAt (Array.length x / 2) x)
    >> Array.map (Tuple.map Set)
    >> Array.map (fun (a, b) -> a ^ b)
    >> Array.map (Set.map priority)
    >> Array.map (Seq.sum)
    >> Array.sum
    >> printfn "%i"
