module Day8

    let isVisible (trees: int list list) ((x, y): int * int): bool =
        let row = trees[x]
        let tree = row[y]
        if (row |> List.take y |> List.forall (fun t -> t < tree)) || (row |> List.skip (y + 1) |> List.forall (fun t -> t < tree)) then true
        else let column = (List.transpose trees)[y] in (column |> List.take x |> List.forall (fun t -> t < tree)) || (column |> List.skip (x + 1) |> List.forall (fun t -> t < tree))

    let processGrid: string list -> int list list =
        List.map (Seq.toList >> List.map (string >> int))

    let mapOnGrid (transform: int * int -> 'b): 'a list list -> 'b list list =
        List.mapi (fun x row -> List.mapi (fun y _ -> transform (x, y)) row)

    let part1 (input: string list): unit =
        let trees = processGrid input
        trees
        |> mapOnGrid (isVisible trees)
        |> List.sumBy (List.filter id >> List.length)
        |> printfn "%d"

    let scenicScore (trees: int list list) ((x, y): int * int): int =
        let row = trees[x]
        let column = (List.transpose trees)[y]
        let tree = row[y]
        let rec countUnblocked (trees: int list): int =
            match (List.tryFindIndex (fun t -> t >= tree) trees) with
            | Some n -> n + 1
            | None -> trees.Length
        let left = row |> List.take y |> List.rev |> countUnblocked
        let right = row |> List.skip (y + 1) |> countUnblocked
        let up = column |> List.take x |> List.rev |> countUnblocked
        let down = column |> List.skip (x + 1) |> countUnblocked
        left * right * up * down

    let part2 (input: string list): unit =
        let trees = processGrid input
        trees
        |> mapOnGrid (scenicScore trees)
        |> List.concat
        |> List.max
        |> printfn "%d"