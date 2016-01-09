module Personal.Account
    let age = 25
    let name = "Mark"
    let dateOfBirth = "01/01/1970"
    let birthDecade = "1970"
    let creditCardNumber = "1234567812345678"
    let cv2 = 123
    let private passPhrase = "SecretPhrase"
    let pin = 4321

    let changePin pin secretPhrase =
        match secretPhrase with
        | x when x = passPhrase -> () // Allow pin change
        | _ -> () // Deny pin change

    let changeSecret passPhrase pin =
        match pin with
        | x when x = pin -> () // Allow passPhrase change
        | _ -> () // Deny passPhrase change

    let pay who amount pin =
        match pin with
        | x when x = pin -> () // Allow payment
        | _ -> () // Stop payment

    let withDraw amount pin =
        match pin with
        | x when x = pin -> () // Allow withdrawal
        | _ -> () // Deny withdrawal