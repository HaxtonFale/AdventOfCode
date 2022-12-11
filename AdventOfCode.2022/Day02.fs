module Day02
    open System.Text.RegularExpressions

    module private RPS =
        type Shape =
            | Rock = 1
            | Paper = 2
            | Scissors = 3

        type Outcome =
            | Win = 6
            | Draw = 3
            | Loss = 0

        let private matchups: (Shape * Shape * Outcome) array = [|
            Shape.Rock, Shape.Paper, Outcome.Win; Shape.Paper, Shape.Scissors, Outcome.Win; Shape.Scissors, Shape.Rock, Outcome.Win;
            Shape.Rock, Shape.Rock, Outcome.Draw; Shape.Paper, Shape.Paper, Outcome.Draw; Shape.Scissors, Shape.Scissors, Outcome.Draw;
            Shape.Rock, Shape.Scissors, Outcome.Loss; Shape.Paper, Shape.Rock, Outcome.Loss; Shape.Scissors, Shape.Paper, Outcome.Loss
            |]

        let findResult (theirs: Shape) (mine: Shape): Outcome =
            matchups |> Array.find (fun (t, m, _) -> (t = theirs) && (m = mine)) |> fun (_, _, result) -> result

        let findPlay (theirs: Shape) (result: Outcome): Shape =
            matchups |> Array.find (fun (t, _, r) -> (t = theirs) && (r = result)) |> fun (_, mine, _) -> mine

        let pattern = Regex(@"^(?<theirs>[A-C]) (?<mine>[X-Z])$", RegexOptions.Compiled)

    let part1 (lines: string list): unit =
        let mapShape (input: string): RPS.Shape =
            match input with
            | "A" -> RPS.Shape.Rock
            | "B" -> RPS.Shape.Paper
            | "C" -> RPS.Shape.Scissors
            | "X" -> RPS.Shape.Rock
            | "Y" -> RPS.Shape.Paper
            | "Z" -> RPS.Shape.Scissors
            | _ -> invalidArg (nameof input) (sprintf "Invalid shape: %s" input)

        let parseLine (line: string): RPS.Shape * RPS.Shape =
            if not (RPS.pattern.IsMatch line) then
                invalidArg (nameof line) (sprintf "Invalid input: '%s'" line)
            else
                let result = RPS.pattern.Match line
                mapShape result.Groups["theirs"].Value, mapShape result.Groups["mine"].Value

        lines |> List.sumBy (parseLine >> (fun (theirs, mine) -> int mine + (RPS.findResult theirs mine |> int))) |> printfn "%d"

    let part2 (lines: string list): unit =
        let mapShape (input: string): RPS.Shape =
            match input with
            | "A" -> RPS.Shape.Rock
            | "B" -> RPS.Shape.Paper
            | "C" -> RPS.Shape.Scissors
            | _ -> invalidArg (nameof input) (sprintf "Invalid shape: %s" input)

        let mapResult (input: string): RPS.Outcome = 
            match input with
            | "X" -> RPS.Outcome.Loss
            | "Y" -> RPS.Outcome.Draw
            | "Z" -> RPS.Outcome.Win
            | _ -> invalidArg (nameof input) (sprintf "Invalid result: %s" input)

        let parseLine (line: string): RPS.Shape * RPS.Outcome =
            if not (RPS.pattern.IsMatch line) then
                invalidArg (nameof line) (sprintf "Invalid input: '%s'" line)
            else
                let result = RPS.pattern.Match line
                mapShape result.Groups["theirs"].Value, mapResult result.Groups["mine"].Value

        lines |> List.sumBy (parseLine >> (fun (theirs, result) -> (RPS.findPlay theirs result |> int) + int result)) |> printfn "%d"