module Day06
    let detect (consecutive: int) (input: char list): int =
        let limit = consecutive - 1
        let rec recurse (queue: char list) (input: char list): int =
            if List.contains input.Head queue || (List.distinct queue).Length < limit then 1 + (recurse (input.Head::(List.take (limit - 1) queue)) input.Tail)
            else 1
        limit + recurse (List.rev (List.take limit input)) (List.skip limit input)

    let part1 (input: string list): unit =
        input.Head |> Seq.toList |> detect 4 |> printfn "%d"

    let part2 (input: string list): unit =
        input.Head |> Seq.toList |> detect 14 |> printfn "%d"
