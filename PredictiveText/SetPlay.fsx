let empty = Set.empty

let toBe = Set.ofList ["to";"be";"or";"not";"to";"be"]

let that = Set.add "that" empty

let thatQuestion = Set.ofList ["that";"is";"the";"question"]

let thatToBeQuest = toBe |> Set.union that |> Set.union thatQuestion
let thatToBeQuest2 = toBe + that + thatQuestion

let toBeDiff = toBe |> Set.difference thatToBeQuest2
let toBeDiff2 = thatToBeQuest2 - that
