module FSharp.TV.Trie

    type NodeFlag =
        | EndOfWord
        | IncompleteWord

    type Trie =
        { children : Map<char, Trie>
          flag : NodeFlag }

    let emptyTrie =
        { children = Map.empty
          flag = IncompleteWord }

    let insertWord word initialTrie =
        let rec innerInsertChars charList currentTrie =
            match charList with
            | [] -> { emptyTrie with flag = EndOfWord }
            | ch::rest ->
                let innerMap = currentTrie.children
                let innerTrie =
                    match innerMap |> Map.tryFind ch with
                    | None -> emptyTrie
                    | Some childTrie -> childTrie
                let newInnerTrie = innerInsertChars rest innerTrie
                let newMap = innerMap |> Map.add ch newInnerTrie
                { currentTrie with children = newMap }
        innerInsertChars (word |> Seq.toList) initialTrie

    let containsWord word trie =
        let rec containsChars charList currentTrie =
             match charList with
             | [] -> Some currentTrie.flag
             | ch::rest -> 
                 match Map.tryFind ch currentTrie.children with
                 | None -> None
                 | Some child -> containsChars rest child
        let wordFound = containsChars (word |> List.ofSeq) trie
        match wordFound with
        | Some IncompleteWord -> false
        | Some EndOfWord -> true
        | None -> false

    let allSuffixes initialPrefix initialTrie = 
        let rec innerAllSuffixes prefix currentTrie currentResults =
            let newResults =
                if currentTrie.flag = EndOfWord then
                    currentResults |> Set.add prefix
                else
                    currentResults

            if currentTrie.children.Count = 0 then
                newResults
            else
                currentTrie.children
                |> Map.fold (
                    fun resultsSoFar ch childTrie ->
                        let newPrefix = prefix + string ch
                        innerAllSuffixes newPrefix childTrie resultsSoFar)
                    newResults

        let results = Set.empty
        innerAllSuffixes initialPrefix initialTrie results

    let tryFindPrefix prefix initialTrie : Option<Trie> =
        prefix
        |> Seq.fold (
            fun trieOpt ch ->
                match trieOpt with
                | None -> None
                | Some trie -> trie.children |> Map.tryFind ch)
            initialTrie

    let tryFindSuffixes prefix trieOpt : List<string> =
        match trieOpt with
        | None -> []
        | Some trie -> trie |> allSuffixes prefix |> Seq.toList

    let autoComplete prefix initialTrie = 

        let currentTrieOpt = Some (initialTrie) |> tryFindPrefix prefix

        let possibleWords = tryFindSuffixes prefix currentTrieOpt

        possibleWords