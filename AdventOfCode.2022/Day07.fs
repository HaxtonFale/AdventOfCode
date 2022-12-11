module Day07
    type ListItem =
        | File of name: string * size: int
        | Directory of name: string

    type Instruction =
        | Move of target: string
        | MoveUp
        | List

    type Node =
        | File of name: string * size: int
        | Directory of name: string * contents: Node list

    let parseInstruction (input: string): Instruction =
        input.Split(' ') |> fun splits -> if splits.[1] = "ls" then List else if splits.[2] = ".." then MoveUp else Move splits.[2]

    let parseListItem (input: string): ListItem =
        input.Split(' ') |> fun splits -> if splits.[0] = "dir" then ListItem.Directory splits.[1] else ListItem.File (splits.[1], (int splits.[0]))

    let makePath (input: string list): string =
        match input with
        | [] -> ""
        | _ -> let x::xs = List.rev input in x + (String.concat "/" xs)

    let rec mapFiles (map: Map<string, ListItem list>) (directoryStack: string list) (lines: string list): Map<string, ListItem list> =
        match lines with
        | [] -> map
        | l::ls ->
            match parseInstruction l with
            | MoveUp -> mapFiles map directoryStack.Tail ls
            | Move target -> mapFiles map (target::directoryStack) ls
            | List ->
                let files = List.takeWhile (fun (s:string) -> not (s.StartsWith("$"))) ls
                let rest = List.skipWhile (fun (s:string) -> not (s.StartsWith("$"))) ls
                let nodes = List.map parseListItem files
                let newMap = Map.add (makePath directoryStack) nodes map
                mapFiles newMap directoryStack rest

    let rec mapTree (ownership: Map<string, ListItem list>) (location: string list): Node =
        let children = Map.find (makePath location) ownership |> List.map (
            fun item ->
                match item with
                | ListItem.File (name, size) -> Node.File (name, size)
                | ListItem.Directory name -> mapTree ownership (name::location)
        )
        Node.Directory (location.Head, children)

    let rec getSize (node: Node): int =
        match node with
        | Node.File (_, size) -> size
        | Node.Directory (_, contents) -> List.sumBy getSize contents

    let rec findSmallerThan (target: int) (root: Node): (Node * int) list =
        let size = getSize root
        match root with
        | Node.File (_, _) -> []
        | Node.Directory (_, contents) -> (if size <= target then [root, size] else []) @ (List.map (findSmallerThan target) contents |> List.concat)

    let rec findGreaterThan (target: int) (root: Node): (Node * int) list =
        let size = getSize root
        match root with
        | Node.File (_, _) -> []
        | Node.Directory (_, contents) -> (if size >= target then [root, size] else []) @ (List.map (findGreaterThan target) contents |> List.concat)

    let printTree: Node -> unit =
        let rec printInner (depth: int) (node: Node) =
            match node with
            | Node.File (name, size) -> printfn "%s- %s (%d)" (String.replicate depth "  ") name size
            | Node.Directory (name, contents) ->
                printfn "%s- %s [%d]" (String.replicate depth "  ") name (getSize node)
                List.map (printInner (depth + 1)) contents |> ignore
        printInner 0
            
    let part1 (input: string list): unit =
        let tree = mapTree (mapFiles Map.empty<string, ListItem list> [] input) ["/"]
        printTree tree
        tree
        |> findSmallerThan 100000
        |> List.sumBy snd
        |> printfn "%d"

    let part2 (input: string list): unit =
        let tree = mapTree (mapFiles Map.empty<string, ListItem list> [] input) ["/"]
        let totalSpace = 70000000
        let minimumFree = 30000000
        let actualFree = totalSpace - (getSize tree)
        tree
        |> findGreaterThan (minimumFree - actualFree)
        |> List.minBy snd
        |> snd
        |> printfn "%d"
