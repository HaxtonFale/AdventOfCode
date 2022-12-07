module Day3
    let priority (item: char): int =
        if System.Char.IsUpper item then
            int item - int 'A' + 27
        else
            int item - int 'a' + 1

    let rec findSingleCommon (l::ls: char list) (r::rs: char list): char =
        if l = r then l
        else if l > r then
            findSingleCommon (l::ls) rs
        else
            findSingleCommon ls (r::rs)

    let rec findAllCommon (left: char list) (right: char list): char list =
        match left, right with
        | [], _ -> []
        | _, [] -> []
        | l::ls, r::rs -> 
            if l = r then (l :: findAllCommon ls rs)
            else if l > r then findAllCommon left rs
            else findAllCommon ls right

    let part1 (lines: string list): unit =
        lines
        |> List.sumBy (List.ofSeq >> List.splitInto 2 >> fun (xs::ys::_) -> 
            let newXs, newYs = List.sort xs, List.sort ys
            findSingleCommon newXs newYs |> priority)
        |> printfn "%d"

    let part2 (lines: string list): unit =
        let rec group (sacks: string list): string list list =
            match sacks with
            | [] -> []
            | xs::ys::zs::rest -> [xs; ys; zs] :: group rest
        lines |> group |> List.map (List.map (List.ofSeq >> List.sort))
        |> List.sumBy (fun [xs; ys; zs] ->
            findAllCommon xs ys |> findAllCommon zs |> List.distinct |> List.exactlyOne |> priority)
        |> printfn "%d"