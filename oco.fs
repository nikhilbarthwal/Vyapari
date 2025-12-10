// Build form content for OCO order
let buildOCOFormContent (accountId: string) (oco: OCOOrder) (duration: Duration) =
    let content =
        [ ("class", "oco")
          ("symbol[0]", oco.FirstOrder.Symbol)
          ("side[0]", orderSideToString oco.FirstOrder.Side)
          ("quantity[0]", string oco.FirstOrder.Quantity)
          ("type[0]", orderTypeToString oco.FirstOrder.Type)
          ("duration[0]", durationToString duration)

          // Second order (leg 1)
          ("symbol[1]", oco.SecondOrder.Symbol)
          ("side[1]", orderSideToString oco.SecondOrder.Side)
          ("quantity[1]", string oco.SecondOrder.Quantity)
          ("type[1]", orderTypeToString oco.SecondOrder.Type)
          ("duration[1]", durationToString duration) ]

    // Add optional prices
    let contentWithPrices =
        content
        |> fun c ->
            match oco.FirstOrder.Price with
            | Some p -> ("price[0]", string p) :: c
            | None -> c
        |> fun c ->
            match oco.FirstOrder.Stop with
            | Some s -> ("stop[0]", string s) :: c
            | None -> c
        |> fun c ->
            match oco.SecondOrder.Price with
            | Some p -> ("price[1]", string p) :: c
            | None -> c
        |> fun c ->
            match oco.SecondOrder.Stop with
            | Some s -> ("stop[1]", string s) :: c
            | None -> c

    new FormUrlEncodedContent(contentWithPrices)

// Place OCO order
let placeOCOOrder
    (apiToken: string)
    (accountId: string)
    (oco: OCOOrder)
    (duration: Duration)
    (sandbox: bool)
    =
    async {
        let baseUrl =
            if sandbox then
                "https://sandbox.tradier.com"
            else
                "https://api.tradier.com"

        let url = sprintf "%s/v1/accounts/%s/orders" baseUrl accountId

        use client = new HttpClient()

        client.DefaultRequestHeaders.Add(
            "Authorization",
            sprintf "Bearer %s" apiToken
        )

        client.DefaultRequestHeaders.Add("Accept", "application/json")

        let content = buildOCOFormContent accountId oco duration

        try
            let! response = client.PostAsync(url, content) |> Async.AwaitTask

            let! responseBody =
                response.Content.ReadAsStringAsync() |> Async.AwaitTask

            if response.IsSuccessStatusCode then
                return Ok responseBody
            else
                return
                    Error(
                        sprintf
                            "Error: %d - %s"
                            (int response.StatusCode)
                            responseBody
                    )
        with ex ->
            return Error(sprintf "Exception: %s" ex.Message)
    }

// Example usage - Bracket existing position with take profit and stop loss
let exampleOCOBracketPosition () =
    // You already own 100 shares of AAPL
    // Set take profit at $180 and stop loss at $170
    let ocoOrder =
        { FirstOrder =
            { Symbol = "AAPL"
              Side = Sell
              Quantity = 100
              Type = Limit
              Price = Some 180.00m
              Stop = None }
          SecondOrder =
            { Symbol = "AAPL"
              Side = Sell
              Quantity = 100
              Type = Stop
              Price = None
              Stop = Some 170.00m } }

    let apiToken = "YOUR_API_TOKEN"
    let accountId = "YOUR_ACCOUNT_ID"

    async {
        let! result = placeOCOOrder apiToken accountId ocoOrder Duration.GTC true

        match result with
        | Ok response -> printfn "OCO bracket order placed successfully: %s" response
        | Error error -> printfn "Failed to place order: %s" error
    }
    |> Async.RunSynchronously

// Example usage - Two entry points (buy at different levels)
let exampleOCOTwoEntries () =
    // Try to buy at $145, but if price jumps to $155, buy at market
    let ocoOrder =
        { FirstOrder =
            { Symbol = "TSLA"
              Side = Buy
              Quantity = 50
              Type = Limit
              Price = Some 145.00m
              Stop = None }
          SecondOrder =
            { Symbol = "TSLA"
              Side = Buy
              Quantity = 50
              Type = Stop
              Price = None
              Stop = Some 155.00m } }

    let apiToken = "YOUR_API_TOKEN"
    let accountId = "YOUR_ACCOUNT_ID"

    async {
        let! result = placeOCOOrder apiToken accountId ocoOrder Duration.Day true

        match result with
        | Ok response ->
            printfn "OCO dual entry order placed successfully: %s" response
        | Error error -> printfn "Failed to place order: %s" error
    }
    |> Async.RunSynchronously

// Example usage - Options position exit strategy
let exampleOCOOptionsExit () =
    // You own 10 call contracts
    // Take profit at $8.00 or cut losses at $3.00
    let ocoOrder =
        { FirstOrder =
            { Symbol = "AAPL250117C00170000" // AAPL Jan 17 2025 $170 Call
              Side = SellToClose
              Quantity = 10
              Type = Limit
              Price = Some 8.00m
              Stop = None }
          SecondOrder =
            { Symbol = "AAPL250117C00170000"
              Side = SellToClose
              Quantity = 10
              Type = Stop
              Price = None
              Stop = Some 3.00m } }

    let apiToken = "YOUR_API_TOKEN"
    let accountId = "YOUR_ACCOUNT_ID"

    async {
        let! result = placeOCOOrder apiToken accountId ocoOrder Duration.GTC true

        match result with
        | Ok response ->
            printfn "OCO options exit order placed successfully: %s" response
        | Error error -> printfn "Failed to place order: %s" error
    }
    |> Async.RunSynchronously

// Example usage - Short position management
let exampleOCOShortPosition () =
    // You have a short position in NVDA
    // Cover at profit ($250) or limit loss ($280)
    let ocoOrder =
        { FirstOrder =
            { Symbol = "NVDA"
              Side = Buy // Buy to cover
              Quantity = 75
              Type = Limit
              Price = Some 250.00m
              Stop = None }
          SecondOrder =
            { Symbol = "NVDA"
              Side = Buy // Buy to cover
              Quantity = 75
              Type = Stop
              Price = None
              Stop = Some 280.00m } }

    let apiToken = "YOUR_API_TOKEN"
    let accountId = "YOUR_ACCOUNT_ID"

    async {
        let! result = placeOCOOrder apiToken accountId ocoOrder Duration.GTC true

        match result with
        | Ok response ->
            printfn "OCO short cover order placed successfully: %s" response
        | Error error -> printfn "Failed to place order: %s" error
    }
    |> Async.RunSynchronously
