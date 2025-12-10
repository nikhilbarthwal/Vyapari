// Build form content for OTOCO order
let buildOTOCOFormContent
    (accountId: string)
    (otoco: OTOCOOrder)
    (duration: Duration)
    =
    let content =
        [ ("class", "otoco")
          ("symbol[0]", otoco.PrimaryOrder.Symbol)
          ("side[0]", orderSideToString otoco.PrimaryOrder.Side)
          ("quantity[0]", string otoco.PrimaryOrder.Quantity)
          ("type[0]", orderTypeToString otoco.PrimaryOrder.Type)
          ("duration[0]", durationToString duration)

          // Take profit leg (leg 1)
          ("symbol[1]", otoco.TakeProfitOrder.Symbol)
          ("side[1]", orderSideToString otoco.TakeProfitOrder.Side)
          ("quantity[1]", string otoco.TakeProfitOrder.Quantity)
          ("type[1]", orderTypeToString otoco.TakeProfitOrder.Type)
          ("duration[1]", durationToString duration)

          // Stop loss leg (leg 2)
          ("symbol[2]", otoco.StopLossOrder.Symbol)
          ("side[2]", orderSideToString otoco.StopLossOrder.Side)
          ("quantity[2]", string otoco.StopLossOrder.Quantity)
          ("type[2]", orderTypeToString otoco.StopLossOrder.Type)
          ("duration[2]", durationToString duration) ]

    // Add optional prices
    let contentWithPrices =
        content
        |> fun c ->
            match otoco.PrimaryOrder.Price with
            | Some p -> ("price[0]", string p) :: c
            | None -> c
        |> fun c ->
            match otoco.PrimaryOrder.Stop with
            | Some s -> ("stop[0]", string s) :: c
            | None -> c
        |> fun c ->
            match otoco.TakeProfitOrder.Price with
            | Some p -> ("price[1]", string p) :: c
            | None -> c
        |> fun c ->
            match otoco.TakeProfitOrder.Stop with
            | Some s -> ("stop[1]", string s) :: c
            | None -> c
        |> fun c ->
            match otoco.StopLossOrder.Price with
            | Some p -> ("price[2]", string p) :: c
            | None -> c
        |> fun c ->
            match otoco.StopLossOrder.Stop with
            | Some s -> ("stop[2]", string s) :: c
            | None -> c

    new FormUrlEncodedContent(contentWithPrices)

// Place OTOCO order
let placeOTOCOOrder
    (apiToken: string)
    (accountId: string)
    (otoco: OTOCOOrder)
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

        let content = buildOTOCOFormContent accountId otoco duration

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

// Example usage
let exampleOTOCOOrder () =
    // Buy 100 shares at market, then set take profit at $52 and stop loss at $48
    let otocoOrder =
        { PrimaryOrder =
            { Symbol = "AAPL"
              Side = Buy
              Quantity = 100
              Type = Market
              Price = None
              Stop = None }
          TakeProfitOrder =
            { Symbol = "AAPL"
              Side = Sell
              Quantity = 100
              Type = Limit
              Price = Some 52.00m
              Stop = None }
          StopLossOrder =
            { Symbol = "AAPL"
              Side = Sell
              Quantity = 100
              Type = Stop
              Price = None
              Stop = Some 48.00m } }

    let apiToken = "YOUR_API_TOKEN"
    let accountId = "YOUR_ACCOUNT_ID"

    async {
        let! result = placeOTOCOOrder apiToken accountId otocoOrder Duration.GTC true

        match result with
        | Ok response -> printfn "Order placed successfully: %s" response
        | Error error -> printfn "Failed to place order: %s" error
    }
    |> Async.RunSynchronously
