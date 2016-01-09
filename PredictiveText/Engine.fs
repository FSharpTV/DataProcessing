module FSharp.TV.Engine

    type NodeFlag =
        | EndOfWord
        | IncompleteWord

    type Trie =
        { children : Map<char, Trie>
          flag : NodeFlag }

    let private emptyTrie =
        { children = Map.empty
          flag = IncompleteWord }

    let private insertWord word initialTrie =
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
     
    let private containsWord word trie =
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

    let private allSuffixes initialPrefix initialTrie = 
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

    let private tryFindPrefix prefix initialTrie : Option<Trie> =
        prefix
        |> Seq.fold (
            fun trieOpt ch ->
                match trieOpt with
                | None -> None
                | Some trie -> trie.children |> Map.tryFind ch)
            initialTrie

    let private tryFindSuffixes prefix trieOpt : List<string> =
        match trieOpt with
        | None -> []
        | Some trie -> trie |> allSuffixes prefix |> Seq.toList

    let private autoComplete prefix initialTrie = 

        let currentTrieOpt = Some (initialTrie) |> tryFindPrefix prefix

        let possibleWords = tryFindSuffixes prefix currentTrieOpt

        possibleWords

    ///This response is returned when a call has been made to GetPossibleWords the property possibleWords will contain a sequence of all the words found, if any. The property dictionary is the Trie in which the words were found in.
    type AutoCompleteResponse =
        { possibleWords : seq<string>
          dictionary : Trie }

    ///This response is returned when a call to Contains is made, it has a property call exists that signifies if the word is in the underlying trie. The property dictionary is the Trie in which the word can be found.
    type ContainsResponse =
        { exists : bool
          dictionary: Trie }

    ///Allow the dictionary to have new words added to it
    let Add word dictionary =
        printfn "Adding word '%s' to the dictionary" word
        let newDict = insertWord word dictionary
        printfn "Word '%s' Added to the dictionary" word
        newDict

    ///Attempt to locate possible words from the given word prefix. For example, given 'th' should return a list containing 'the'
    let GetPossibleWords prefix dictionary =
        printfn "Attempting to autocomplete for %A." prefix
        let possibleWords = autoComplete prefix dictionary |> List.toSeq
        { possibleWords = possibleWords; dictionary = dictionary }

    ///Check if the dictionary contains the given word.
    let Contains word dictionary =
        printfn "Check if '%A' exists in the predictive text engine" word
        let exists = containsWord word dictionary
        { exists = exists; dictionary = dictionary }