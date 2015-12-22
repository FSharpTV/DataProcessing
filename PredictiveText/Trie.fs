module FSharp.TV.Trie

    type NodeFlag =
        | EndOfWord
        | IncompleteWord

    type Trie =
        { flag : NodeFlag }