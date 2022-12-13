module Day12.Part2

module String =
    let contains (c : char) (s : string) = s.Contains(c)
    let indexOf (c : char) (s : string) = s.IndexOf(c)

let loadHeightmap =
    Array.map (Array.ofSeq)
    >> Array.transpose
    >> array2D
    >> Array2D.map (function 'S' -> 0 | 'E' -> 25 | x -> int x - 97)

let find c input =
    let y = input |> Seq.findIndex (String.contains c)
    let x = input |> Seq.item y |> String.indexOf c
    (x, y)

let findCandidates heightmap (x, y) =
    seq { (x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y) }
    |> Seq.filter (fun (u, v) -> u >= 0 && u < Array2D.length1 heightmap && v >= 0 && v < Array2D.length2 heightmap)
    |> Seq.filter (fun (u, v) -> heightmap[u, v] > heightmap[x, y] - 2)

type Node = { Position : (int32 * int32); Parent : Node option }

let rec iterate i heightmap test explored = function
| head::_ when test head -> head
| head::tail ->
    let edges = findCandidates heightmap head.Position |> Seq.except explored
    let explored = edges |> Set.ofSeq |> Set.union explored
    let pending = edges |> Seq.map (fun edge -> { Position = edge; Parent = Some head }) |> List.ofSeq
    iterate (i + 1) heightmap test explored (tail @ pending)

let rec walkBack steps = function
| { Parent = Some parent } as node -> walkBack (node::steps) parent
| { Parent = None } -> steps

let findRoute heightmap test start =
    let visited = Set.singleton start
    let pending = [{ Position = start; Parent = None }]
    iterate 0 heightmap test visited pending |> walkBack []

let run (input : string array) =
    let heightmap = loadHeightmap input
    let start = find 'E' input
    findRoute heightmap (fun node -> let (x, y) = node.Position in heightmap[x, y] = 0) start
    |> (printfn "%d" << List.length)
