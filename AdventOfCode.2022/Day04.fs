module Day04
    type Section = int * int

    let contains ((lStart, lEnd): Section) ((rStart, rEnd): Section): bool = lStart <= rStart && lEnd >= rEnd || lStart >= rStart && lEnd <= rEnd

    let overlaps ((lStart, lEnd): Section) ((rStart, rEnd): Section): bool = (lStart <= rStart && rStart <= lEnd) || (lStart <= rEnd && rEnd <= lEnd)

    let parse (line: string): Section * Section =
        line.Split ',' |> Array.map (fun sect -> (sect.Split '-' |> Array.map int |> fun [|x;y|] -> (x, y))) |> fun [|x;y|] -> (x, y)

    let part1: string list -> unit =
        List.map parse >> List.filter (fun (left, right) -> contains left right) >> (fun l -> l.Length) >> printfn "%d"

    let part2: string list -> unit =
        List.map parse >> List.filter (fun (left, right) -> overlaps left right || overlaps right left) >> (fun l -> l.Length) >> printfn "%d"
