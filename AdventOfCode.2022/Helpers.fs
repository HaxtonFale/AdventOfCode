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

    module Inputs =
        let read (day: int) (part: int): string list = [
            let filename = $"Inputs\d{day}p{part}.txt"
            use reader = new System.IO.StreamReader(filename)
            while not reader.EndOfStream do
                yield reader.ReadLine ()
        ]
