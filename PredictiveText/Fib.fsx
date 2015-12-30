// Fibonacci
let rec fib lim n acc =
    let next = ((List.head acc) + n)
    if lim <= 1 then acc
    else fib (lim-1) next (n :: acc)

fib 6 1 [1]

let rec fib2 lim n acc =
    let next = ((List.head acc) + n)
    match next with
    | _ when lim <= 1 -> acc
    | x -> fib2 (lim-1) x (n :: acc)

fib2 6 1 [1]
fib2 7 1 [1]

// With auxillary function
let getFib lim =
    let rec fib2 lim n acc =
        let next = ((List.head acc) + n)
        match next with
        | _ when lim <= 1 -> List.rev acc
        | x -> fib2 (lim-1) x (n :: acc)
    fib2 lim 1 [1]

getFib 7