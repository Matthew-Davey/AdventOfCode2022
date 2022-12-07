module Day7.Part1

open System.Collections.Generic

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match (input, pattern)
    if m.Success then Some (List.tail [ for g in m.Groups -> g.Value ]) else None

type Node =
| Directory of (Node option * string * List<Node>)
| File of (Node * string * int32)

let rec findRoot = function
| Directory (None, _, _) as node -> node
| Directory (Some parent, _, _) -> findRoot parent

let rec findSize = function
| File (_, _, size) -> size
| Directory (_, _, children) -> Seq.fold (fun acc node -> acc + (findSize node)) 0 children

let rec fold fn acc = function
| File _ as f -> fn acc f
| Directory (_, _, children) as node -> Seq.fold (fold fn) (fn acc node) children

let choose fn node =
    let impl acc node = match fn node with Some x -> x::acc | None -> acc
    fold impl [] node

let recreateFilesytem input =
    Seq.fold (fun (Directory (parent, _, children) as node) input ->
        match input with
        | "$ cd /" -> findRoot node
        | "$ cd .." -> parent.Value
        | Regex "\\$ cd (.+)" [name] ->
             Seq.find (function | Directory (_, child, _) -> child = name | _ -> false) children
        | Regex "dir (.+)" [name;] ->
            children.Add (Directory (Some node, name, List<Node>()))
            node
        | Regex "(\\d+) (.+)" [size; name] ->
            children.Add (File (node, name, int size))
            node
        | _ -> node
    ) (Directory (None, "/", List<Node>())) input
    |> findRoot

let run : string array -> unit =
    recreateFilesytem
    >> choose (function
        | File _ -> None
        | Directory _ as node ->
            let size = findSize node
            if size <= 100000 then Some size else None)
    >> (printfn "%i" << Seq.sum)
