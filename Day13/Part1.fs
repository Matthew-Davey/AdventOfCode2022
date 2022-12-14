module Day13.Part1
open FParsec

type Packet = List of Packet list | Int of int32 | Absent

let ppacket, ppacketR = createParserForwardedToRef()
let plist = ((pchar '[') >>. (sepBy ppacket (pchar ',')) .>> pchar ']')
do ppacketR.Value <- choice [pint32 |>> Int; plist |>> List]

let balance a b padding =
    let pad n xs = xs @ List.replicate (n - List.length xs) padding
    let len = max (List.length a) (List.length b) in pad len a, pad len b

let rec compare = function
| List [], List [] -> None
| List a, List b when List.length a <> List.length b ->
    let a, b = balance a b Absent in compare (List a, List b)
| List (ahead::atail), List (bhead::btail) ->
    match compare (ahead, bhead) with
    | Some result -> Some result
    | None -> compare (List atail, List btail)
| Int a, Int b when a < b -> Some true
| Int a, Int b when a > b -> Some false
| Int a, Int b when a = b -> None
| List a, Int b -> compare (List a, (List [Int b]))
| Int a, List b -> compare (List [Int a], List b)
| Absent, _ -> Some true
| _, Absent -> Some false

let run : string array -> unit =
    Seq.chunkBySize 3
    >> Seq.map (Seq.take 2)
    >> Seq.map (List.ofSeq)
    >> Seq.map (List.map (run ppacket >> function Success (x, _, _) -> x))
    >> Seq.mapi (fun i [a; b] -> i + 1, compare (a, b))
    >> Seq.filter (fun (_, x) -> x = Some true)
    >> (Seq.sum << Seq.map fst)
    >> printfn "%d"
