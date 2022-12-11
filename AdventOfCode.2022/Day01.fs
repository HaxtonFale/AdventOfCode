module Day01
    let solve (top: int): string list -> unit =
        Helpers.List.partitionOnElement "" >> List.map (List.sumBy int) >> List.sortDescending >> List.take top >> List.sum >> printfn "%d"

    let part1 = solve 1
    let part2 = solve 3