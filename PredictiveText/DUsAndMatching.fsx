// Example 1

type Fruit = | Berry | Citrus | Nut

let peanut = Nut
let raspberry = Berry
let orange = Citrus

let describeTheFruit fruit =
    match fruit with
    | Berry -> printfn "Careful of the seeds"
    | Citrus -> printfn "Good source of Vitamin D"
    | Nut -> printfn "Best when salted"

describeTheFruit peanut
describeTheFruit raspberry
describeTheFruit orange


// Example 2    
        
type Direction =
    | Left
    | Right
    | Up
    | Down
    | Backward
    | Forward

type Person = { moving: Direction }

let status person =
    match person.moving with
    | Left -> printfn "moving left"
    | Right -> printfn "moving right"
    | Up -> printfn "moving up"
    | Down -> printfn "moving down"
    | Backward -> printfn "moving backward"
    | Forward -> printfn "moving forward"
    person

let turn direction person =
    match direction with
    | Left -> { person with moving = Left }
    | Right -> { person with moving = Right }
    | Up -> { person with moving = Up }
    | Down -> { person with moving = Down }
    | Backward -> { person with moving = Backward }
    | Forward -> { person with moving = Forward }

let pete = { moving = Forward }

pete 
|> status |> turn Left 
|> status |> turn Up
|> status |> turn Backward
|> status |> turn Left
|> status |> turn Right
|> status |> turn Up

// Example 3

type Symbol =
    | Number of int
    | Letter of char
    | Fn of float*(float -> float)

let write symbol =
    match symbol with
    | Number i -> printfn "Matched number %i and multiplying it!" i; printfn "%i" (i * 3)
    | Letter l -> printfn "Matched letter %c" l; printfn "Reprinting: %c" l
    | Fn (n,f) -> printfn "Matched a function, calling: %f" (f n)

for i in 1..10 do Number i |> write

for c in "Hello world" do Letter c |> write

for f in 1.0..5.0 do Fn (f, fun i -> exp i) |> write