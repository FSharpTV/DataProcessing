(*
Kirk has started an away mission to the Planet Zog, McCoy as always hates
transporters so he likes to get it over and done with as quick as possible.
Spock as always has seen something 'fascinating'.
Use the Transporter (Pattern Matching function) to beam up the crew, 
it already knows to inform us if it cannot obtain a lock on the target.
Step 1: Beam up McCoy first, print a message to the FSI stating he is 
in the Med Bay
Step 2: Next Kirk says, "Beam me up Scotty!", so beam him up next
Step 3: The red shirt crew members are always dying, this is no exception, 
trigger a malfunction in the transporter whenever a red shirt is beamed up
Step 4: Finally, beam up Spock after he has finished analysing whatever
it is that has 'fascinated' him.
Step 5: When you have completed the repair of the transporter, transport 
crew back aboard the enterprise, see Step 5 at the bottom..
I have added Sulu to the pattern match to give you an idea of how 
to make the repairs to the transporter.
*)
type Crew = YellowShirt | RedShirt | BlueShirt | GreenShirt

let spock = BlueShirt
let mcCoy = GreenShirt
let sulu = YellowShirt

// Can't obtain a transporter lock on nothing!
let nothing = []
// McCoy hates transporters, lets get him off the planet first
let mcCoyFirst = [mcCoy; BlueShirt; YellowShirt]
// Transporting Kirk on his own
let kirk = [YellowShirt]
// Spock will return last as he is currently 'fascinated' by something
let spockLast = [YellowShirt; GreenShirt; spock;]
// Bring home Sulu any time
let suluAnytime = [YellowShirt; sulu; YellowShirt;]
// those poor red shirted crew members never survive :(
let accidentWaitingToHappen = [RedShirt] 

// Your task is to fix the transporter and get all the crew off the planet safely,
// well, almost all the crew, the poor red shirts just wont survive the transporter!
let transport crew =
    match crew with
    // Step 1: Transport McCoy first directly to the Med Bay
    | who::_ when who = mcCoy -> printfn "McCoy has been beamed directly to med bay"
    // Step 2: Try transporting Kirk on his own
    | [YellowShirt] -> printfn "Beam me up Scotty!"
    // Step 3: Try transporting a RedShirt
    | crew when List.contains RedShirt crew -> printfn "Oh no! Capt'n we lost his transporter signature!"
    // Step 4: Now bring Spock back to Transporter Room 1
    | YellowShirt::GreenShirt::spock::[] -> printfn "Spock has re-materialised in transporter room 1"    
    | _::YellowShirt::_ -> printfn "Sulu is safely back aboard the Enterprise."
    | _ -> printfn "Can't obtain a transporter lock, Capt'n!"

(*
Step 5: When you are ready highlight the following 6 lines and execute in the FSI:
transport mcCoyFirst
transport kirk
transport spockLast
transport accidentWaitingToHappen
transport suluAnytime
transport nothing
*)