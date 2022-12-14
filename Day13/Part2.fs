module Day13.Part2
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
    Seq.filter (fun x -> String.length x > 0)
    >> Seq.append ["[[2]]"; "[[6]]"]
    >> Seq.map (run ppacket >> function Success (x, _, _) -> x)
    >> Seq.sortWith (fun a b -> match compare (a, b) with Some true -> -1 | Some false -> 1 | None -> 0)
    >> Seq.mapi (fun i x -> (i + 1, x))
    >> Seq.filter (fun (_, x) -> x = List [List [Int 2]] || x = List [List [Int 6]])
    >> Seq.map fst
    >> (printfn "%A" << Seq.fold (*) 1)
