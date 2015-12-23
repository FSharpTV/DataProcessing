let empty = Map.empty

let mapped = empty.Add("Key","Value")

mapped.["Key"]
mapped.["not there"]

mapped.TryFind("not there")
mapped |> Map.tryFind "Key"

let selfPwrs = [for f in 1.0..7.0 do yield (f, f**f)] |> Map.ofList
selfPwrs |> Map.tryFind 7.0

let findPower x =
    match selfPwrs |> Map.tryFind x with
    | Some f -> printfn "%f" f
    | None -> printfn "Number not found."

let capitals =
    [("Australia", "Canberra"); ("Canada", "Ottawa"); ("China", "Beijing");
        ("Denmark", "Copenhagen"); ("Egypt", "Cairo"); ("Finland", "Helsinki");
        ("France", "Paris"); ("Germany", "Berlin"); ("India", "New Delhi");
        ("Japan", "Tokyo"); ("Mexico", "Mexico City"); ("Russia", "Moscow");
        ("Slovenia", "Ljubljana"); ("Spain", "Madrid"); ("Sweden", "Stockholm");
        ("Taiwan", "Taipei"); ("USA", "Washington D.C.")]
    |> Map.ofList

let findCapital country =
    match capitals |> Map.tryFind country with
    | Some cap -> printfn "The capital of %s is %s." country cap
    | None -> printfn "Unknown country specified."
