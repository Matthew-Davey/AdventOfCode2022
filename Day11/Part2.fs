module Day11.Part2

type Monkey = {
    Items : bigint list
    Operation : bigint -> bigint
    Test : bigint -> bool
    Targets : int32 * int32
    Inspections : int64
}

let loadMonkey (input : string array) = {
    Items = (input[1][18..]).Split ", " |> Seq.map int32 |> Seq.map bigint |> List.ofSeq
    Operation = match (input[2][23], input[2][25..]) with
                | ('*', "old") -> fun x -> (x * x) % bigint 9699690
                | ('*', n) -> let y = bigint (int32 n) in fun x -> x * y
                | ('+', n) -> let y = bigint (int32 n) in fun x -> x + y
    Test = let y = bigint (int32 (input[3][21..])) in fun x -> x % y = bigint.Zero
    Targets = (int32 (input[4][29..]), int32 (input[5][30..]))
    Inspections = 0L
}

let rec turn i monkeys =
    match Array.item i monkeys with
    | { Items = [] } -> monkeys
    | { Items = item::items } as monkey ->
        let worry = (monkey.Operation item)
        let target = if monkey.Test worry then fst monkey.Targets else snd monkeys[i].Targets
        monkeys[i] <- { monkey with Items = items; Inspections = monkey.Inspections + 1L }
        monkeys[target] <- { monkeys[target] with Items = monkeys[target].Items @ [worry] }
        turn i monkeys

let rounds num monkeys =
    let round (monkeys : Monkey array) =
        [0..(Array.length monkeys - 1)] |> Seq.fold (fun monkeys i -> turn i monkeys) monkeys
    [0..num - 1] |> Seq.fold (fun monkeys _ -> round monkeys) monkeys

let run : string array -> unit =
    Array.chunkBySize 7
    >> Array.map loadMonkey
    >> rounds 10000
    >> Array.sortByDescending (fun monkey -> monkey.Inspections)
    >> fun monkeys -> monkeys[0].Inspections * monkeys[1].Inspections
    >> printfn "%A"
