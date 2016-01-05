let __COMPLETE_ME__<'T> : 'T = raise <| new System.NotImplementedException("You must implement this to continue.")

open System.IO
open System.Text
open System.Text.RegularExpressions

let words = 
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "soliloquy.txt")) // Make sure you save the text file in the source directory
    |> Seq.collect // Using collect as the the fun will return an sequence of sequences, collect will flatten into a single sequence
        (fun line -> 
            seq 
                { for matches in Regex("\w+").Matches(line) -> // the Regex splits the line into individual words
                  matches.Value.ToUpper() }) // Use toUpper so that the words are the same
     |> Seq.toList

// Step 1: Verify the words in the test exist in the body of text that has been loaded, 
// you can used List.contains. Then you can run the tests: "runTests()"
let slings()   = words |> List.contains "SLINGS"
let shuffled() = words |> List.contains "SHUFFLED"
let brigand()  = words |> List.contains "BRIGAND"

// Step 2: Build a "Set" from the "words" list
let set() : Set<string> = Set.ofList words 

// Step 3: Use "set" you just created instead of words directly, see if you can
// find the rather *relevant* hidden word in the text after it has been mapped.
// You can leave the indexes, assume they are already correct.
let findMe = 
    [44;119;50;5;108;98]
    |> List.map (fun i -> (set() |> Set.toList).[i].[0], (set() |> Set.toList).[i]) 
    |> List.map (fun (k,v) -> sprintf "[%c] - %s" k v)

// Run these when your are ready
let runTests() =
    let areEquivalent description actual expected =
        if actual <> expected
        then failwith (sprintf "Test Failed: %A - %A is not equivalent to %A" description expected actual)
        else printfn "Test Passed: %A" description

    areEquivalent
        "Should contain the word 'SLINGS'"
        (slings())
        (true)

    areEquivalent
        "Should contain the word 'SHUFFLED'"
        (shuffled())
        (true)

    areEquivalent
        "Should not contain the word 'BRIGAND'"
        (brigand())
        (false)

    areEquivalent
        "Should not contain 'MORE LIFE SAY BE PAUSE WE'"
        (findMe = ["[M] - MORE"; "[L] - LIFE"; "[S] - SAY"; "[B] - BE"; "[P] - PAUSE"; "[W] - WE"])
        (false)