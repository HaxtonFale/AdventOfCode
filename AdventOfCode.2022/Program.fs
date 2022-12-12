open CommandLine

let run (day: int) (part: int): unit =
    match day, part with
    | 1, 1 ->
        Day01.part1 (Helpers.Inputs.read day 1)
    | 1, 2 ->
        Day01.part2 (Helpers.Inputs.read day 1)
    | 2, 1 ->
        Day02.part1 (Helpers.Inputs.read day 1)
    | 2, 2 ->
        Day02.part2 (Helpers.Inputs.read day 1)
    | 3, 1 ->
        Day03.part1 (Helpers.Inputs.read day 1)
    | 3, 2 ->
        Day03.part2 (Helpers.Inputs.read day 1)
    | 4, 1 ->
        Day04.part1 (Helpers.Inputs.read day 1)
    | 4, 2 ->
        Day04.part2 (Helpers.Inputs.read day 1)
    | 5, 1 ->
        Day05.part1 (Helpers.Inputs.read day 1)
    | 5, 2 ->
        Day05.part2 (Helpers.Inputs.read day 1)
    | 6, 1 ->
        Day06.part1 (Helpers.Inputs.read day 1)
    | 6, 2 ->
        Day06.part2 (Helpers.Inputs.read day 1)
    | 7, 1 ->
        Day07.part1 (Helpers.Inputs.read day 1)
    | 7, 2 ->
        Day07.part2 (Helpers.Inputs.read day 1)
    | 8, 1 ->
        Day08.part1 (Helpers.Inputs.read day 1)
    | 8, 2 ->
        Day08.part2 (Helpers.Inputs.read day 1)
    | 9, 1 ->
        Day09.part1 (Helpers.Inputs.read day 1)
    | 9, 2 ->
        Day09.part2 (Helpers.Inputs.read day 1)
    | _ ->
        if part > 2 || part < 1 then
            invalidArg (nameof part) (sprintf "Invalid part number: %d" part)
        else
            invalidArg (nameof day) (sprintf "Invalid day: %d" day)

type Options = {
    [<Option('d', "day", Required = false)>] day: int option
    [<Option('p', "part", Required = false)>] part: int option
}

let maxDay = 8
[<EntryPoint>]
let main (args:string array) =
    match CommandLine.Parser.Default.ParseArguments<Options> args with
    | :? Parsed<Options> as parsed ->
        let days =
            match parsed.Value.day with
            | None -> [1..maxDay]
            | Some d -> [d]
        let parts =
            match parsed.Value.part with
            | None -> [1; 2]
            | Some p -> [p]
        for day in days do
            for part in parts do
                run day part
        0
    | :? NotParsed<Options> as notParsed ->
        notParsed.Errors |> printfn "%A"
        1
    | _ -> -1