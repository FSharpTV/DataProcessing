
(*
Please ensure you have familiarised yourself with the tests included at the bottom of the page.
Please start with Step 1 below, the instructions are next to the place where the implementation
needs to be written. When you are done you can run the tests by sending your entire script to 
the FSI then type runTests();; into the FSI to execute the tests that have been provided to help
guide you along as you attempt to implement auto-complete functionality. Good luck.
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
      
        let buildSuffix newResults currentTrie =
            let possibleWords results key trie = 
                // put the parent nodes's char in front
                let newPrefix = prefix + string key 
                // recurse into children
                innerAllSuffixes newPrefix trie results
            currentTrie.children 
            |> Map.fold possibleWords newResults 

        if currentTrie.children.Count = 0 then
            newResults // Done
        else
            buildSuffix newResults currentTrie

    let results = Set.empty
    innerAllSuffixes initialPrefix initialTrie results 

// Start - Implementation
(*

Execute script to FSI and call runTests();; to see what needs to be fixed first

Step 1: Fully implement the prefix finding function called 'tryFindPrefix' that take 2 inputs:-
    - prefix : string
    - initialTrie : Option<Trie>
Use "prefix |> Seq.fold (fun ?) initialTrie" and pattern match on the 'initialTrie', remembering
it is an 'Option' type. 
    None -> None
    Some trie -> on the trie's children 'Map.tryFind' the 'char' that is currently being folded over. 
Return the result of the fold into a value called 'currentTrieOpt' ('Option<Trie>').

The 'tryFindPrefix' funtion will fold over 'prefix' pick each 'char' on at a time and will attempt to
build up a 'Trie' for each of the characters successfully located, when it is finished it will have 
built up all the nodes it has located into an 'Option<Trie>' which should eventually be passed on to 
the Suffix Lookup function that we will use in Step 5 further down this script.
*)     
let tryFindPrefix prefix initialTrie : Option<Trie> =
    prefix
    |> Seq.fold (
        fun trieOpt ch ->
            match trieOpt with
            | None -> None
            | Some trie -> trie.children |> Map.tryFind ch)
        initialTrie

// Execute script to FSI and call 'runTests();;' to verify that you have implemented the function.
// You should see 2 passing tests and 1 NotImplementedException.

(*
Make sure you have done Steps 1 first!

Step 2: Fully implement the function called 'tryFindSuffixes' it should take 2 inputs:-
    - prefix : string
    - trieOpt : Option<Trie>
The 2nd input 'trieOpt' is the 'currentTrieOpt' ('Option<Trie>') passed out from 'tryFindPrefix', this will
be used to search for suffixes using the 'allSuffixes' function we wrote in the last lecture. 
Perfom a pattern match over 'trieOpt';
     None -> empty list
     Some trie -> goes to 'allSuffixes' with 'prefix' and the result is converted to 'List'.
*)
let tryFindSuffixes prefix trieOpt : List<string> =
    match trieOpt with
    | None -> []
    | Some trie -> trie |> allSuffixes prefix |> Seq.toList

// Execute script to FSI and call runTests();; to verify that you have implemented the function.
// You should have 5 passing tests and a NotImplementedException.

(*

Step 3: Implement the auto-complete function, called 'autoComplete' and give it two inputs:-
    - prefix : string
    - initialTrie : Trie

Auto-complete does 2 things; 
Step 4: 1st) Prefix Lookup - it will take the 'initialTrie' it is given, wrap it in an option and 
send it along with "prefix" into a function (Step 1) that will look up the prefix to see 
if it exists in the Trie. If it does exist, it will return a Option<Trie> for the prefix 
it was provided, capture it in a value called 'currentTrieOpt'.
 
Step 5: 2nd) Suffix Lookup - the 'currentTrieOpt' ('Option<Trie>') returned from the Prefix Lookup 
function will be provided to the 'tryFindSuffixes' function. The prefix is effectively prepended 
to the suffixes to provide a selection of possible words in the form of 'List<string>' assign the
result to a new value called "possibleWords".
*)
let autoComplete prefix initialTrie = 

    let currentTrieOpt = Some (initialTrie) |> tryFindPrefix prefix // Step 4: Call tryFindPrefix 

    let possibleWords = tryFindSuffixes prefix currentTrieOpt // Step 5: Call tryFindSuffixes

    possibleWords // Step 6: Return possibleWords


// Execute script to FSI and call runTests();; to verify that you have implemented the function
// You should have all tests passing and can now see the auto-complete list being sent back to the FSI
// End - Implementation

// Start - Test Data
let trie1 = 
    emptyTrie
    |> insertWord "can" 
    |> insertWord "car" 
    |> insertWord "carriage" 
    |> insertWord "carry" 
    |> insertWord "cat"
    |> insertWord "dare"
    |> insertWord "dank"
    |> insertWord "dent"
    |> insertWord "dearth"
    |> insertWord "dog"
    |> insertWord "donkey"
    |> insertWord "zoo"
    |> insertWord "zoology"
    |> insertWord "jam"
    |> insertWord "jeans"
    |> insertWord "jello"
    |> insertWord "pea"
    |> insertWord "peanut"

let peTrieOpt = 
    Some
      {children =
        Map
          [('a',
            {children =
              Map
                [('n',
                  {children =
                    Map [('u', {children = Map [('t', {children = Map [];
                                                       flag = EndOfWord;})];
                                flag = IncompleteWord;})];
                   flag = IncompleteWord;})];
             flag = EndOfWord;})];
       flag = IncompleteWord;}

let dTrieOpt =
  Some
    {children =
      Map
        [('a',
          {children =
            Map
              [('n', {children = Map [('k', {children = Map [];
                                             flag = EndOfWord;})];
                      flag = IncompleteWord;});
               ('r', {children = Map [('e', {children = Map [];
                                             flag = EndOfWord;})];
                      flag = IncompleteWord;})];
           flag = IncompleteWord;});
         ('e',
          {children =
            Map
              [('a',
                {children =
                  Map
                    [('r',
                      {children =
                        Map
                          [('t', {children = Map [('h', {children = Map [];
                                                         flag = EndOfWord;})];
                                  flag = IncompleteWord;})];
                       flag = IncompleteWord;})];
                 flag = IncompleteWord;});
               ('n', {children = Map [('t', {children = Map [];
                                             flag = EndOfWord;})];
                      flag = IncompleteWord;})];
           flag = IncompleteWord;});
         ('o',
          {children =
            Map
              [('g', {children = Map [];
                      flag = EndOfWord;});
               ('n',
                {children =
                  Map
                    [('k',
                      {children =
                        Map
                          [('e', {children = Map [('y', {children = Map[];
                                                         flag = EndOfWord;})];
                                  flag = IncompleteWord;})];
                       flag = IncompleteWord;})];
                 flag = IncompleteWord;})];
           flag = IncompleteWord;})];
     flag = IncompleteWord;}
// Start - Test Data

// Start - Testing
let runTests() =
    let areEquivalent description actual expected =
        if actual <> expected
        then failwith (sprintf "Test Failed: %A - %A is not equivalent to %A" description expected actual)
        else printfn "Test Passed: %A" description

    areEquivalent
        "Should contain prefix for 'pe'"         // Description
        (tryFindPrefix "pe" (Some trie1)).IsSome // Actual
        (true)                                   // Expected

    areEquivalent
        "Should NOT contain prefix for 'ha'"
        (tryFindPrefix "ha" (Some trie1)).IsNone 
        (true)

    areEquivalent
        "Should contain 2 suffixes for 'pe'"
        (tryFindSuffixes "pe" peTrieOpt).Length
        (2)

    areEquivalent
        "Should contain 6 suffixes for 'd'"
        (tryFindSuffixes "d" dTrieOpt).Length
        (6)

    areEquivalent
        "Should contain 0 suffixes for 'q'"
        (tryFindSuffixes "q" None).Length
        (0)

    areEquivalent
        "Should return '[\"zoo\";\"zoology\"]' when auto-completing on 'z'"
        (autoComplete "z" trie1)
        (["zoo";"zoology"])

    areEquivalent
        "Should return '[]' when auto-completing on 's'"
        (autoComplete "s" trie1)
        ([])