(* 
I have provided some of the code already you need to write the recursive "allSuffixes" function.
There is a step by step (7 steps) specification below and I have created some crude tests so
that you can verify what you are coding alongside the tests. To run the tests, simply execute the 
code you have written, then you can type "runTests()" in the FSI.

Step 0: It is important that you spend a little time trying to understand the code and the tests 
at the bottom of the page to familiarise yourself, it will be crucial to ensure you make as few
mistakes as possible but that you also take some essential learnings forward to the next lectures.

Step 1: Complete the auxilliary function "allSuffixes" that has 2 inputs:-
 - initialPrefix : string
 - initialTrie : Trie
 
Step 2: Write the inner recursive function "innerAllSuffixes" which takes 3 inputs:-
 - prefix : string
 - currentTrie : Trie
 - currentResults : Set<string>

Step 3: Inside the recursive function you need to check if the "currentTrie.flag" is "EndOfWord"
If it is then you should add "prefix" to the "Set" "currentResults" else return it as is.
Assign the resulting value of the "if" expression to another value called "newResults"
which we'll change later but for now simply return it.

Step 4: We are almost ready to call our inner rec fn, assign an empty "Set" to "results" using "Set.empty"
Step 5: Now call the inner rec fn with the correct arguments then run the tests

Step 6: If the "currentTrie" has zero children you need to return the "newResults" value we 
created in Step 3 earlier only when this condition is met. Go to Step 7 directly.

Step 7: This part is crucial! While the "currentTrie" has children you should pass them into the 
function "Map.fold", which can automatically deconstruct the Trie's children "Map" into a "key" 
and "trie" for you. While we are folding we need to pass in the prefix which can be constructed using 
the previous "char". Construct "newPrefix" by concatenating "prefix" and "key" (don't forget to cast the "key" 
to "string" as it is a "char"), to find each suffix.

"Map.fold" (for our "Map<char, Trie>") has a signature of:

      "folder -> state:Set<string> -> table : Map<char, Trie>"

the "folder" has a signature of:
      
      "'b:Set<string> -> 'c:char -> 'd:Trie -> 'b:Set<string>"

This means you can fold similar to the following code, essentially adding "char" items to "'b" our "currentResults" "Set":

      "trie.children |> Map.fold (fun res key trie -> <call the rec fn here>) currentResults"

If you send all the code to FSI now and call "runTests()" now you will see failures, but after 
implementing Step 7 correctly you should see passing tests.

*)
let __COMPLETE_ME__<'T> : 'T = raise <| new System.NotImplementedException("You must implement this to continue.")

// Start: Code so far:
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
// End: Code so far:

// Start: Test Variables
// NOTE: Using mutable values (or variables) here to facilitate our tests, later we will discuss
// them, but for now, you need to know you can change their value using the '<-' operator,
// which permits the value to change. You can remove these when copying to library.

// Use in Step 3: Use in the True case like so: "endOfWordCount <- endOfWordCount+1"
// We look for the value in our tests to ensure we count the number of words found
let mutable endOfWordCount = 0

// Use in Step 7: Use in the False case like so: "recurseCount <- recurseCount+1"
// We look for the value in our tests to ensure we can count the number of times
// we have recursed through the inner recursive function the expected amount.
let mutable recurseCount = 0
// End: Test Variables

// Start: Implement the specification
// Step 1: Complete the auxilliary function "allSuffixes" that has 2 inputs:-
// - initialPrefix : string
// - initialTrie : Trie
let allSuffixes initialPrefix initialTrie = 
    // Step 2: Write the inner recursive function "innerAllSuffixes" which takes 3 inputs:-
    // - prefix : string
    // - currentTrie : Trie
    // - currentResults : Set<string>
    let rec innerAllSuffixes prefix currentTrie currentResults =
    
    // Step 3: Inside the recursive function you need to check if the "currentTrie.flag" is "EndOfWord"
    // If it is then you should add "prefix" to the "Set" "currentResults" else return it as is.
    // Assign the resulting value of the "if" expression to another value called "newResults"
    // which we'll change later but for now simply return it.
    // Don't forget to add "endOfWordCount <- endOfWordCount+1"
        let newResults =
            if currentTrie.flag = EndOfWord then
                endOfWordCount <- endOfWordCount+1
                currentResults |> Set.add prefix
            else
                currentResults
        

    // DO STEPS 4 & 5 FIRST THEN COME BACK TO STEPS 6 & 7
    // Step 6: If the "currentTrie" has zero children you need to return the "newResults" value we 
    // created in Step 3 earlier only when this condition is met. Go to Step 7 directly.
        if currentTrie.children.Count = 0 then
            newResults
    // Step 7: This part is crucial! While the "currentTrie" has children you should pass them into the 
    // function "Map.fold", which can automatically deconstruct the Trie's children "Map" into a "key" 
    // and "trie" for you. While we are folding we need to pass in the prefix which can be constructed using 
    // the previous "char". Construct "newPrefix" by concatenating "prefix" and "key" (don't forget to cast the "key" 
    // to "string" as it is a "char"), to find each suffix. Don't forget to add "recurseCount <- recurseCount+1"
        else
            recurseCount <- recurseCount+1
            currentTrie.children
            |> Map.fold (
                fun resultsSoFar ch childTrie ->
                    let newPrefix = prefix + string ch
                    innerAllSuffixes newPrefix childTrie resultsSoFar)
                newResults
    // Step 4: We are almost ready to call our inner rec fn, assign an empty "Set" to "results" using "Set.empty"
    let results = Set.empty
    // Step 5: Now call the inner rec fn with the correct arguments then run the tests.
    // Hightlight all the code and send to FSI then call "runTests()" when you have implemented this step
    innerAllSuffixes initialPrefix initialTrie results
// End: Implement the specification

// We will create a complete function for this later, allow autocomplete on substrings.
let naiveAutoComplete (prefix:char) (initialTrie:Trie) :Set<string> = 
    let possibleWords = 
        initialTrie.children.[prefix]
        |> allSuffixes (string prefix)
    possibleWords

// Start Testing - highlight ALL and execute in FSI then in the FSI type "runTests()"

// TestData
let trie1 = 
    emptyTrie
    |> insertWord "can" 
    |> insertWord "cat" 
    |> insertWord "cane" 
    |> insertWord "dog" 
    |> insertWord "dot"

// words starting with "sp"
let trieSP = 
    emptyTrie
    |> insertWord "spa" 
    |> insertWord "span" 
    |> insertWord "spend" 
    |> insertWord "spent"

let triePWords = 
    emptyTrie
    |> insertWord "pea" 
    |> insertWord "price" 
    |> insertWord "poise"
// Single completeword
let completeTrieOfA = emptyTrie |> insertWord "a"
// Single incomplete word
let incompleteTrieOfB = { emptyTrie with children = Map.ofList ['b',emptyTrie] }
// Words of various length
let completeTrieOfLa = emptyTrie |> insertWord "la"
let completeTrieOfTree = emptyTrie |> insertWord "tree"
let completeTrieOfEight = emptyTrie |> insertWord "eight"
let completeTrieOfNinety = emptyTrie |> insertWord "ninety"
// Tests

let runTests() =
    // Crude method to allow testing
    let areEquivalent description actual expected =
        if actual <> expected
        then failwith (sprintf "Test Failed: %A - %A is not equivalent to %A" description expected actual)
        else printfn "Test Passed: %A" description
    // reset all mutable variables back to ZERO
    let resetTests () = 
        recurseCount <- 0
        endOfWordCount <- 0

    // Test by executing the naiveAutoComplete and reading the endOfWordCount variable
    areEquivalent
        "Should contain 0 complete word(s) when searching with 'b'"
        (naiveAutoComplete 'b' incompleteTrieOfB |> ignore; endOfWordCount)
        (0)
    resetTests()

    // Test by executing the naiveAutoComplete and reading the endOfWordCount variable
    areEquivalent
        "Should contain 1 complete word(s) when searching with 'a'"
        (naiveAutoComplete 'a' completeTrieOfA |> ignore; endOfWordCount)
        (1)
    resetTests()

    // Test by executing the naiveAutoComplete and reading the endOfWordCount variable
    areEquivalent
        "Should contain 2 complete word(s) when searching with 'd'"
        (naiveAutoComplete 'd' trie1 |> ignore; endOfWordCount)
        (2)
    resetTests()

    // Test by executing the naiveAutoComplete and reading the endOfWordCount variable
    areEquivalent
        "Should contain 4 complete word(s) when searching with 's'"
        (naiveAutoComplete 's' trieSP |> ignore; endOfWordCount)
        (4)
    resetTests()

    // Test by executing the naiveAutoComplete and reading the recurseCount variable
    areEquivalent
        "Should recurse 0 times for the word 'a'"
        (naiveAutoComplete 'a' completeTrieOfA |> ignore; recurseCount)
        (0)
    resetTests()

    areEquivalent
        "Should recurse 1 times for the word 'la'"
        (naiveAutoComplete 'l' completeTrieOfLa |> ignore; recurseCount)
        (1)
    resetTests()


    areEquivalent
        "Should recurse 3 times for the word 'tree'"
        (naiveAutoComplete 't' completeTrieOfTree |> ignore; recurseCount)
        (3)
    resetTests()

    areEquivalent
        "Should recurse 4 times for the word 'eight'"
        (naiveAutoComplete 'e' completeTrieOfEight |> ignore; recurseCount)
        (4)
    resetTests()

    areEquivalent
        "Should recurse 5 times for the word 'ninety'"
        (naiveAutoComplete 'n' completeTrieOfNinety |> ignore; recurseCount)
        (5)
    resetTests()

    // Testing by executing the naiveAutoComplete and reading the recurseCount variable
    // Worth noting that the trie branch levels are counted once not the different chars, 
    // i.e., 'g' and 't' in dog and dot will be counted in the same step. Same for 'o', 
    // so the value should be 2, 1 for 'o' and 1 for 'g' and 't'. This is because they have
    // the same parent.
    areEquivalent
        "Should recurse 2 times for the words '[\"dog\"; \"dot\"]'"
        (naiveAutoComplete 'd' trie1 |> ignore; recurseCount)
        (2)
    resetTests()

    areEquivalent
        "Should recurse 3 times for the words '[\"can\"; \"cane\"; \"cat\"]'"
        (naiveAutoComplete 'c' trie1 |> ignore; recurseCount)
        (3)
    resetTests()

    areEquivalent
        "Should recurse 5 times for the words '[\"spa\"; \"span\"; \"spend\"; \"spent\"]'"
        (naiveAutoComplete 's' trieSP |> ignore; recurseCount)
        (5)
    resetTests()

    // Here the recurseCount will be higher as the only parent node shared is the first node
    // of 'p'. So count 'e', 'r' and 'o' as 1, then 1 for each following char for each word.
    // 'e','r' and 'o' = 1, 'a'=1, 'i', 'c' and 'e' = 3, 'i', 's', and 'e' = 3 - 1+1+3+3=8 
    areEquivalent
        "Should recurse 8 times for the words '[\"pea\"; \"price\"; \"poise\"]'"
        (naiveAutoComplete 'p' triePWords |> ignore; recurseCount)
        (8)
    resetTests()

    // "Sets" are equivalent if they contain the exactly the same items, order doesn't matter
    areEquivalent 
        "Should locate 'a' in a trie where only 'a' exists."
        (naiveAutoComplete 'a' completeTrieOfA) // Get the actual suffixes for 'a'
        (Set.ofList ["a"]) // a set with only 'a' which is what we expect
    resetTests()

    areEquivalent 
        "Should locate [\"can\"; \"cane\"; \"cat\"] in a trie where the words beginning with 'c' exist."
        (naiveAutoComplete 'c' trie1)     // actual
        (Set.ofList ["can";"cat";"cane"]) // expected
    resetTests()

    areEquivalent 
        "Should locate [\"dog\"; \"dot\"] in a trie where the words beginning with 'd' exist."
        (naiveAutoComplete 'd' trie1)     // actual
        (Set.ofList ["dog"; "dot"]) // expected
    resetTests()


