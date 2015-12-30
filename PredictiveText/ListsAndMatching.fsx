let empty = []
let headAndEmptyTail = ["first"]
let headAndTail = ["first";"second";"third"]
let fourItems = ["first";"second";"third";"fourth"]

let revAndGetHead l =
    l |> List.rev |> List.head

let printInfo lst =
    match lst with
    | f::s::t::fourth::_ -> printfn "You found %A and the value should be 'fourth'" fourth
    | l when List.contains "fourth" l -> printfn "When reversed, the head is %A" (revAndGetHead l)
    | head::[] -> printfn "The 'head' is: %A and the 'tail' is empty []" head
    | head::tail -> printfn "The 'head' is: %A and the 'tail' is: %A" head tail
    | [] -> printfn "you gave me an empty list []"
    | _ -> printfn "No match"

printInfo empty
printInfo headAndEmptyTail
printInfo headAndTail
printInfo fourItems

