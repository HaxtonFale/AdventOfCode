module Day10

    type Command =
        | AddX of int
        | Noop

    let parse (line: string): Command =
        let splits = line.Split(' ')
        match splits[0] with
        | "addx" -> AddX (int splits[1])
        | "noop" -> Noop

    let execute ((r::rs): int list) (cmd: Command): int list =
        match cmd with
        | Noop -> r::r::rs
        | AddX x -> (r + x)::r::r::rs

    let part1 (input: string list): unit =
        input
        |> List.map parse
        |> List.fold execute [1]
        |> List.rev
        |> (fun list -> list.[19] * 20 + list.[59] * 60 + list.[99] * 100 + list.[139] * 140 + list.[179] * 180 + list.[219] * 220)
        |> printfn "%d"

    let part2 (input: string list): unit =
        let rows = input |> List.map parse |> List.fold execute [1] |> List.rev |> List.chunkBySize 40
        for row in rows do
            for (index, pos) in List.indexed row do
                if abs (pos - index) <= 1 then printf "#" else printf " "
            printfn ""