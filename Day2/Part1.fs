module Day2.Part1

type Move = Rock | Paper | Scissors
type Move with static member parse = function | 'A'|'X' -> Rock | 'B'|'Y' -> Paper | 'C'|'Z' -> Scissors
type Move with member x.score = match x with | Rock -> 1 | Paper -> 2 | Scissors -> 3
type Move with member x.beats = match x with | Rock -> Scissors | Paper -> Rock | Scissors -> Paper

type Result = Win | Draw | Lose
type Result with member x.score = match x with | Win -> 6 | Draw -> 3 | Lose -> 0

let mapTuple f (x, y) = (f x, f y)

let playRound : Move * Move -> int = function
    | (theirMove, ourMove) when ourMove.beats = theirMove -> ourMove.score + Win.score
    | (theirMove, ourMove) when theirMove.beats = ourMove -> ourMove.score + Lose.score
    | (theirMove, ourMove) when theirMove = ourMove       -> ourMove.score + Draw.score

let run: string array -> unit =
    Array.map (fun x -> (x.[0], x.[2]))
    >> Array.map (mapTuple Move.parse)
    >> Array.map playRound
    >> Array.sum
    >> printfn "%i"
