// Example 1
type SimpleShape = 
    | Triangle 
    | Square 
    | Circle

let guessSimpleShape shape =
    match shape with
    | Triangle -> printfn "A triangle?"
    | Square   -> printfn "Some kind of rectangle?"
    | Circle   -> printfn "Circle?"

let triangle = guessSimpleShape Triangle
let square   = guessSimpleShape Square
let circle   = guessSimpleShape Circle

// Example 2

type Shape = Sides of int

let noSides    = Sides 0
let oneSide    = Sides 1
let twoSides   = Sides 2
let threeSides = Sides 3
let fourSides  = Sides 4
let ninetyNine = Sides 99
let threeSixty = Sides 360

let guessShape shape =
    match shape with
    | Sides 0 -> printfn "You can't trick me."
    | Sides x when x < 3 -> printfn "Only %i sides, must be line of sorts?" x
    | Sides 3 -> printfn "A triangle?"
    | Sides 4 -> printfn "Some kind of rectangle?"
    | Sides 360 -> printfn "Circle?"
    | _ -> printfn "Must be a polygon?"

guessShape noSides
guessShape oneSide
guessShape twoSides
guessShape threeSides
guessShape fourSides
guessShape ninetyNine
guessShape threeSixty

// Example 3

type Fruit = | Berry | Citrus | Nut

let peanut    = Nut
let raspberry = Berry
let orange    = Citrus

let describeTheFruit fruit =
    match fruit with
    | Berry  -> printfn "Careful of the seeds"
    | Citrus -> printfn "Good source of Vitamin D"
    | Nut    -> printfn "Best when salted"

describeTheFruit peanut
describeTheFruit raspberry
describeTheFruit orange

// Example 4  
        
type Direction =
    | Left
    | Right
    | Up
    | Down
    | Backward
    | Forward

type Car = { moving: Direction }

let status car =
    match car.moving with
    | Left -> printfn "moving left"
    | Right -> printfn "moving right"
    | Up -> printfn "moving up"
    | Down -> printfn "moving down"
    | Backward -> printfn "moving backward"
    | Forward -> printfn "moving forward"
    car

let turn direction car =
    match direction with
    | Left -> { car with moving = Left }
    | Right -> { car with moving = Right }
    | Up -> { car with moving = Up }
    | Down -> { car with moving = Down }
    | Backward -> { car with moving = Backward }
    | Forward -> { car with moving = Forward }

let peteInHisCar = { moving = Forward }

peteInHisCar 
|> status |> turn Left 
|> status |> turn Up
|> status |> turn Backward
|> status |> turn Left
|> status |> turn Right
|> status |> turn Up

// Example 5 - Random nonsense

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