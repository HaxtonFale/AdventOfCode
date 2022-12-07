module Day5
    open Helpers.List
    open System.Text.RegularExpressions

    // amount, source, destination
    type Instruction = int * int * int

    let parseStacks (input: string list): char list list =
        input
        |> List.map (fun line -> line |> Seq.toList |> List.chunkBySize 4 |> List.map (fun chunk -> chunk.[1]))
        |> List.transpose
        |> List.map (List.takeWhile (fun c -> c <> ' ') >> List.rev)

    let parseInstructions (input: string list): Instruction list =
        let instructionPattern = new Regex(@"move (?<amount>\d+) from (?<source>\d+) to (?<destination>\d+)")
        input |> List.map (fun line ->
            let patternMatch = instructionPattern.Match line
            if (patternMatch.Success) then (int (patternMatch.Groups.["amount"].Value), (int (patternMatch.Groups.["source"].Value) - 1), (int (patternMatch.Groups.["destination"].Value) - 1))
            else invalidArg (nameof line) (sprintf "Invalid line: %s" line)
        )

    let rec executeInstructions (stacks: char list list) (instructions: Instruction list): char list list =
        let rec executeInstruction (stacks: char list list) (instruction: Instruction): char list list =
            match instruction with
            | (0, _, _) -> stacks
            | (amount, source, destination) ->
                let element = stacks.[source].Head
                let result = stacks |> List.mapi (fun index stack ->
                    if index = source then stack.Tail
                    else if index = destination then element::stack
                    else stack
                )
                executeInstruction result (amount - 1, source, destination)
        match instructions with
        | [] -> stacks
        | i::is -> executeInstructions (executeInstruction stacks i) is

    let formatHeads: char list list -> string = List.fold (fun str stk -> sprintf "%s%c" str stk.Head) ""

    let part1 (input: string list): unit =
        let stacks, instructions = input |> partitionOnElement "" |> fun [stacksLines; instructionLines] -> parseStacks (List.rev stacksLines).Tail, parseInstructions instructionLines
        executeInstructions stacks instructions |> formatHeads |> printfn "%s"

    let rec executeInstructionsBulk (stacks: char list list) (instructions: Instruction list): char list list =
        let executeInstruction (stacks: char list list) ((amount, source, destination): Instruction): char list list =
            let elements = List.take amount stacks.[source]
            stacks |> List.mapi (fun index stack ->
                if index = source then List.skip amount stack
                else if index = destination then elements @ stack
                else stack
            )
        match instructions with
        | [] -> stacks
        | i::is -> executeInstructionsBulk (executeInstruction stacks i) is

    let part2 (input: string list): unit =
        let stacks, instructions = input |> partitionOnElement "" |> fun [stacksLines; instructionLines] -> parseStacks (List.rev stacksLines).Tail, parseInstructions instructionLines
        executeInstructionsBulk stacks instructions |> formatHeads |> printfn "%s"