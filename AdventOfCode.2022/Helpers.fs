module Helpers
    module List =
        /// <summary>Splits a given list into a list of lists on elements matching the predicate. These elements are not included in the result.</summary>
        let rec partitionOn (predicate: 'a -> bool) (input: 'a list): 'a list list =
            match input with
            | [] -> []
            | _ ->
                match input |> List.tryFindIndex predicate with
                | None -> [input]
                | Some index -> input |> List.splitAt (index) |> fun (current, rest) -> current :: partitionOn predicate rest.Tail

        /// <summary>Splits a given list into a list of lists on occurrences of the provided value. These elements are not included in the result.</summary>
        let partitionOnElement (pivot: 'a): 'a list -> 'a list list = partitionOn (fun elem -> elem = pivot)

        let rec splitWhen (predicate: 'a -> bool) (input: 'a list): 'a list list =
            match input with
            | [] -> [[]]
            | x::xs -> if predicate x then let y::ys = splitWhen predicate xs in (x::y)::ys else ([]::[input])

        let rec takeLast (input: 'a list): 'a =
            match input with
            | [] -> invalidArg (nameof input) "Invalid argument: empty list"
            | [x] -> x
            | _::xs -> takeLast xs

    module Inputs =
        let read (day: int) (part: int): string list = [
            use reader = new System.IO.StreamReader($"Inputs\d{day}p{part}.txt")
            while not reader.EndOfStream do
                yield reader.ReadLine ()
        ]
