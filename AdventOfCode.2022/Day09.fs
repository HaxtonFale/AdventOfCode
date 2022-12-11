module Day09
    type Move =
    | Up
    | Down
    | Right
    | Left

    type Position = int * int

    let adjacent ((x1, y1): Position) ((x2, y2): Position): bool = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

    let moveTail ((xt, yt): Position) ((xh, yh): Position): Position = (xt + sign (xh - xt), yt + sign (yh - yt))

    let parse (line: string): Move * int =
        let splits = line.Split(' ')
        let distance = int splits[1]
        match splits[0] with
        | "U" -> Up, distance
        | "D" -> Down, distance
        | "L" -> Left, distance
        | "R" -> Right, distance

    let moveOnce ((x, y): Position) (move: Move): Position =
        match move with
        | Up -> x + 1, y
        | Down -> x - 1, y
        | Left -> x, y - 1
        | Right -> x, y + 1

    let rec processMove ((h::hs, t::ts): Position list * Position list) ((move, distance): Move * int): Position list * Position list =
        if distance = 0 then (h::hs, t::ts)
        else
            let movedHead = moveOnce h move
            let movedTail = if adjacent movedHead t then t else moveTail t movedHead
            processMove (movedHead::h::hs, movedTail::t::ts) (move, distance - 1)

    let part1 (input: string list): unit =
        input
        |> List.map parse
        |> List.fold processMove ([0,0], [0,0])
        |> snd
        |> List.distinct
        |> List.length
        |> printfn "%d"