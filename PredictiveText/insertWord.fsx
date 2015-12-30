// When you are done the next two lines will not be required
type WhatType<'T> = { Unknown : 'T }
let __COMPLETE_ME__<'T> : 'T = raise <| new System.NotImplementedException("You must implement this to continue.")


type NodeFlag =
    | EndOfWord
    | IncompleteWord

(*
Step 1: The Trie type below will need to be completely implemented. For this you will need to
utilise all that you have learned in the previous videos regarding the Map type. In short, you
should create a "Map" type that uses a "char" as the key and a "Trie" (itself) as the value,
making this a recursive type because it is self referencing.
*)
type Trie =
    { children : Map<char,Trie> // Use the correct type here
      flag     : NodeFlag }

(*
Step 2: The "emptyTrie" record we have here is not fully implemented. Make the children a "Map"
and initialise it to an empty "Map" it will automatically correspond to the signature defined 
in the type "Trie" in Step 1 above
*)
let emptyTrie = {
    children = Map.empty
    flag     = IncompleteWord
    }

(*
The function should permit us to recursively insert individual characters into the trie.
In order for this to be possible, you will need to first complete Step 1 above!

Step 3: In the pattern match you see below, the first pattern is an empty list "[]".
This should be a clone of "emptyTrie" with the "NodeFlag" set to "EndOfWord", this will 
only be reached when the end of a sequence of characters has been processed.

Step 4: On the second match: "ch::rest" we will take the next character in the 
sequence and use that to check for the pre-existence in the "currentTrie":
    (assign children to map) 

Step 5: We can then check map to see if "ch" exists (use "Map.tryFind") then we need to 
traverse that node and any of the children it has, this is where recursion comes into play.

Step 6: Use the "rest" segment of the first pattern match to check the sub-trie.

Step 7: Next we need to use "Map.add" on "innerMap" the "ch" and the "newInnerTrie".

Step 8: Finally, we will return a new record which will be a clone of "currentTrie" with 
children set to the "newMap" value.
*)

(*
TEST YOUR FUNCTION
*)

let insertedDirectly = innerInsertChars ['M';'a';'r';'k'] emptyTrie

(* 
Step 9: Before you can copy this into the library you should put this auxillary 
function around the recursive function in order to provide an interface to the 
recursive function that is intuitive to the developer. It is better to hide some
of the implementation for a cleaner interface, allowing the developer to send in
a word instead of a "seq<char>" as it is far more friendlier.
*)
let insertWord word initialTrie =
    // Step 9: *CUT*+paste the "innerInsertChars" function here then do Step 10 below.
    let rec innerInsertChars charList currentTrie =
        match charList with
        // Step 3: This is reached when all the chars from a word have been fully inserted
        | [] -> { emptyTrie with flag = EndOfWord }
        // Currently inserting characters, end of word not reached
        | ch::rest ->
            // Step 4: Get the child Map
            let innerMap = currentTrie.children
            // Use the "innerMap" to get the Trie
            let innerTrie =
                // Step 5: Check to see if the "innerTrie" has a tail for the corresponding "char"
                match innerMap |> Map.tryFind ch with
                | None -> emptyTrie
                | Some childTrie -> childTrie
            // Step 6: Check all the children (RECURSE)
            let newInnerTrie = innerInsertChars rest innerTrie
            // Step 7: Create a new "Map" and add the character "ch" as key with "newInnerTrie" as value
            let newMap = innerMap |> Map.add ch newInnerTrie
            // Step 8: return a clone of the "currentTrie" with the "newMap" as the children
            { currentTrie with children = newMap }
    innerInsertChars (word |> Seq.toList) initialTrie

(*
Step 10: Create your own gist on gist.github.com, use the "insertWord" function to 
add your name (replace my name with your name) and tweet us your gist to @AwesomeFSharp
with the hash tag #recursive
*)
let yourNameTrie = insertWord "Mark" emptyTrie

(*
The output should look similar to this, with the correct nesting:

val yourNameTrie : Trie =
  {children =
    map
      [('M',
        {children =
          map
            [('a',
              {children =
                map [('r', {children = map [('k', {children = map [];
                                                   flag = EndOfWord;})];
                            flag = IncompleteWord;})];
               flag = IncompleteWord;})];
         flag = IncompleteWord;})];
   flag = IncompleteWord;}
*)

(* Highlight the following code to see the "insertWord" function chained together to add many words
 let trie1 = 
    emptyTrie
    |> insertWord "can" 
    |> insertWord "cat" 
    |> insertWord "cane" 
    |> insertWord "dog" 
    |> insertWord "dot"
    
// This is the resulting trie:
//
//    val trie1 : Trie =
//      {children =
//        map
//          [('c',
//            {children =
//              map
//                [('a',
//                  {children =
//                    map [('n', {children = map [('e', {children = map [];
//                                                       flag = EndOfWord;})];
//                                flag = EndOfWord;}); ('t', {children = map [];
//                                                            flag = EndOfWord;})];
//                   flag = IncompleteWord;})];
//             flag = IncompleteWord;});
//           ('d',
//            {children =
//              map
//                [('o',
//                  {children =
//                    map [('g', {children = map [];
//                                flag = EndOfWord;}); ('t', {children = map [];
//                                                            flag = EndOfWord;})];
//                   flag = IncompleteWord;})];
//             flag = IncompleteWord;})];
//       flag = IncompleteWord;}
    
*)