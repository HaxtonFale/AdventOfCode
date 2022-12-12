module Day11
    open System.Text.RegularExpressions
    open Helpers.List

    let monkeyPattern = Regex(@"Monkey (?<number>\d):\n  Starting items: (?<startingItems>[\d, ]+)\n  Operation: new = old (?<operation>+|*) (?<param>\d+|old)\n  Test: divisible by (?<divisor>\d+)\n    If true: throw to monkey (?<true>\d)\n    If false: throw to monkey (?<false>\d)")

    type Monkey(number: int, items: int list, operation: int -> int, test: int -> bool, trueTarget: int, falseTarget: int, itemsInspected: int) =
        member this.receive (item: int): Monkey =
            Monkey (number, items@[item], operation, test, trueTarget, falseTarget, itemsInspected)

        member this.turn (monkeys: Monkey list): Monkey list =
            let processTurn (monkeys: Monkey list) (item: int): Monkey list =
                let finalWorry = (operation item) / 3
                let target = if test finalWorry then trueTarget else falseTarget
                List.mapi (fun (index:int) (monkey:Monkey) ->
                    if index = number then Monkey (number, items.Tail, operation, test, trueTarget, falseTarget, itemsInspected + 1)
                    else if index = target then (monkey.receive item)
                    else monkey
                ) monkeys

            List.fold processTurn monkeys items

    let processMonkey (input: string): Monkey =
        let matchObject = monkeyPattern.Match input
        if not matchObject.Success then invalidArg (nameof input) "Invalid Monkey definition"
        else
            let number = int matchObject.Groups["number"].Value
            let startingItems = matchObject.Groups["startingItems"].Value.Split(", ") |> Array.map int |> Array.toList
            let operation = (fun old -> (if matchObject.Groups["operation"].Value = "+" then (+) else (*)) old (if matchObject.Groups["param"].Value = "old" then old else (int matchObject.Groups["param"].Value)))
            let test = (fun n -> n % (int matchObject.Groups["divisor"].Value) = 0)
            let trueTarget = int matchObject.Groups["true"].Value
            let falseTarget = int matchObject.Groups["false"].Value
            Monkey (number, startingItems, operation, test, trueTarget, falseTarget, 0)

    let part1 (input: string list) =
        let initialMonkeys = input |> partitionOnElement "" |> List.map ((String.concat "\n") >> processMonkey)
        let finalMonkeys = List.fold (fun monkeys _ -> )
