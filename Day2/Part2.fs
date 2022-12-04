module Day2.Part2

type Move = Rock | Paper | Scissors
type Move with static member parse = function | 'A' -> Rock | 'B' -> Paper | 'C' -> Scissors
type Move with member x.score = match x with | Rock -> 1 | Paper -> 2 | Scissors -> 3
type Move with member x.beats = match x with | Rock -> Scissors | Paper -> Rock | Scissors -> Paper
type Move with member x.loses = match x with | Rock -> Paper | Paper -> Scissors | Scissors -> Rock

type Result = Win | Draw | Lose
type Result with member x.score = match x with | Win -> 6 | Draw -> 3 | Lose -> 0

type Cheat = ForceLose | ForceDraw | ForceWin
type Cheat with static member parse = function | 'X' -> ForceLose | 'Y' -> ForceDraw | 'Z' -> ForceWin

let mapTuple f1 f2 (x, y) = (f1 x, f2 y)

let applyCheat : (Move * Cheat) -> (Move * Move) = function
    | (theirMove, ForceWin)  -> (theirMove, theirMove.loses)
    | (theirMove, ForceLose) -> (theirMove, theirMove.beats)
    | (theirMove, ForceDraw) -> (theirMove, theirMove)

let playRound : Move * Move -> int = function
    | (theirMove, ourMove) when theirMove = ourMove       -> ourMove.score + Draw.score
    | (theirMove, ourMove) when theirMove.beats = ourMove -> ourMove.score + Lose.score
    | (theirMove, ourMove) when ourMove.beats = theirMove -> ourMove.score + Win.score

let run : string array -> unit =
    Array.map (fun x -> (x.[0], x.[2]))
    >> Array.map (mapTuple Move.parse Cheat.parse)
    >> Array.map applyCheat
    >> Array.map playRound
    >> Array.sum
    >> printfn "%i"
