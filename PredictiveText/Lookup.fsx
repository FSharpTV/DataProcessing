(*

=======================================================
READ THE FOLLOWING STEPS FIRST, THERE ARE ONLY 6 STEPS!
=======================================================
1. Write an auxiliary function called containsWord taking two inputs: 
 - The first input will be called word and it is a type of string
 - The second input will be called trie and it is a type of Trie
2. Write an internal recursive function called containsChars taking two inputs:
 - The first input will be called charList and it is a type of char list
 - The second input will be called currentTrie and it is a type of Trie
3. Pattern match on the charList, look for [] & ch::rest  this will return a Trie option 
4. When the charList has been fully recursed over, return the currentTrie.flag in the pattern match for []
5. Inside the ch::rest match perform a tryFind on currentTrie to see if ch is there; when there are Some child recursively call back into containsChar with rest and child
6. Call containsChars and capture the result in wordFound and pattern match over it and return false unless there are Some EndOfWord


========================================
PSEUDOCODE to help remember the process:
========================================
containsWord word trie // Auxiliary function
  containsChars charList currentTrie // Recursive function
    if charList is not empty
    then split into ch and rest // using a pattern match
      // This returns option type of Trie
      val childTrie = currentTrie.children.tryFind(ch) // Map.tryFind ch currentTrie.children
      if childTrie is None
      then return None
      // use pattern match to extract the value from this option
      else return containsChar tail childTrie.value 
    else return currentTrie.flag as Option
  
  val wordFound = containsChars (word.ToChars) trie // Call the recursive function internally
  if wordFound is Some IncompleteWord
  then return false
  else if wordFound is None
    return false
  else
    return true // Only if Some EndOfWord


*)
let __COMPLETE_ME__<'T> : 'T = raise <| new System.NotImplementedException("You must implement this to continue.")


// Code so far:
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

let trie1 = 
    emptyTrie
    |> insertWord "can" 
    |> insertWord "cat" 
    |> insertWord "cane" 
    |> insertWord "dog" 
    |> insertWord "dot"

// Complete the following and copy it into trie.fs
let containsWord word trie = // Auxiliary function
    let rec containsChars charList currentTrie = // Recursive function
         match charList with
         | [] -> Some currentTrie.flag
         | ch::rest -> 
             match Map.tryFind ch currentTrie.children with
             | None -> None
             | Some child -> containsChars rest child
        // use 'Map.tryFind' on 'currentTrie' looking for a child for 'ch' (RECURSE)
    let wordFound = containsChars (word |> List.ofSeq) trie
    //match 'wordFound' and only return 'true' when 'Some EndOfWord'
    match wordFound with
    | Some IncompleteWord -> false
    | Some EndOfWord -> true
    | None -> false

// Execute these into the FSI and check to see if the values are as expected
let catTest   = containsWord "cat" trie1   // returns true
let dogTest   = containsWord "dog" trie1   // returns true
let humanTest = containsWord "human" trie1 // returns false