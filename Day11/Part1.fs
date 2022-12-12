module Day11.Part1

type Monkey = {
    Items : int32 list
    Operation : int32 -> int32
    Test : int32 -> bool
    Targets : int32 * int32
    Inspections : int32
}

let loadMonkey (input : string array) = {
    Items = (input[1][18..]).Split ", " |> Seq.map int |> List.ofSeq
    Operation = match (input[2][23], input[2][25..]) with
                | ('*', "old") -> fun x -> x * x
                | ('*', n) -> let y = int n in fun x -> x * y
                | ('+', n) -> let y = int n in fun x -> x + y
    Test = let y = int (input[3][21..]) in fun x -> x % y = 0
    Targets = (int (input[4][29..]), int (input[5][30..]))
    Inspections = 0
}

let rec turn i monkeys =
    match Array.item i monkeys with
    | { Items = [] } -> monkeys
    | { Items = item::items } as monkey ->
        let worry = (monkey.Operation item) / 3
        let target = if monkey.Test worry then fst monkey.Targets else snd monkey.Targets
        monkeys[i] <- { monkey with Items = items; Inspections = monkey.Inspections + 1 }
        monkeys[target] <- { monkeys[target] with Items = monkeys[target].Items @ [worry] }
        turn i monkeys

let rounds n monkeys =
    let round monkeys =
        [0..(Array.length monkeys - 1)] |> Seq.fold (fun monkeys i -> turn i monkeys) monkeys
    [0..n - 1] |> Seq.fold (fun monkeys _ -> round monkeys) monkeys

let run : string array -> unit =
    Array.chunkBySize 7
    >> Array.map loadMonkey
    >> rounds 20
    >> Array.sortByDescending (fun monkey -> monkey.Inspections)
    >> fun monkeys -> monkeys[0].Inspections * monkeys[1].Inspections
    >> printfn "%A"
