module Day5.Part1

module String = let split (separator : char) (x : string) = x.Split separator

let parseStacks =
    Seq.takeWhile ((<>) "")
    >> Seq.transpose
    >> Seq.map (Seq.filter System.Char.IsLetter)
    >> Seq.filter (not << Seq.isEmpty)
    >> Seq.map (List.ofSeq)
    >> Array.ofSeq

let parsePlan =
    Seq.skipWhile ((<>) "")
    >> Seq.skip 1
    >> Seq.map (String.split ' ')
    >> Seq.collect (fun xs -> Seq.replicate (int xs[1]) (1, int xs[3]-1, int xs[5]-1))

let applyMove (stacks : char list array) (n, a, b) =
    stacks |> Array.updateAt b (stacks[a][..n-1] @ stacks[b])
           |> Array.updateAt a (stacks[a][n..])

let run (input : string array) =
    let stacks = parseStacks input
    let plan = parsePlan input

    Seq.fold applyMove stacks plan
    |> Array.map (List.head)
    |> System.String
    |> printfn "%s"
