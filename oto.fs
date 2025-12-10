module Tradier

open System
open System.Net.Http
open System.Text.Json

// Helper functions
let orderSideToString =
    function
    | Buy -> "buy"
    | BuyToOpen -> "buy_to_open"
    | BuyToClose -> "buy_to_close"
    | Sell -> "sell"
    | SellShort -> "sell_short"
    | SellToOpen -> "sell_to_open"
    | SellToClose -> "sell_to_close"

let orderTypeToString =
    function
    | Market -> "market"
    | Limit -> "limit"
    | Stop -> "stop"
    | StopLimit -> "stop_limit"

let durationToString =
    function
    | Day -> "day"
    | GTC -> "gtc"
    | PreMarket -> "pre"
    | PostMarket -> "post"

// Build form content for OTO order
let buildOTOFormContent (accountId: string) (oto: OTOOrder) (duration: Duration) =
    let content =
        [ ("class", "oto")
          ("symbol[0]", oto.PrimaryOrder.Symbol)
          ("side[0]", orderSideToString oto.PrimaryOrder.Side)
          ("quantity[0]", string oto.PrimaryOrder.Quantity)
          ("type[0]", orderTypeToString oto.PrimaryOrder.Type)
          ("duration[0]", durationToString duration)

          // Secondary order (leg 1)
          ("symbol[1]", oto.SecondaryOrder.Symbol)
          ("side[1]", orderSideToString oto.SecondaryOrder.Side)
          ("quantity[1]", string oto.SecondaryOrder.Quantity)
          ("type[1]", orderTypeToString oto.SecondaryOrder.Type)
          ("duration[1]", durationToString duration) ]

    // Add optional prices
    let contentWithPrices =
        content
        |> fun c ->
            match oto.PrimaryOrder.Price with
            | Some p -> ("price[0]", string p) :: c
            | None -> c
        |> fun c ->
            match oto.PrimaryOrder.Stop with
            | Some s -> ("stop[0]", string s) :: c
            | None -> c
        |> fun c ->
            match oto.SecondaryOrder.Price with
            | Some p -> ("price[1]", string p) :: c
            | None -> c
        |> fun c ->
            match oto.SecondaryOrder.Stop with
            | Some s -> ("stop[1]", string s) :: c
            | None -> c

    new FormUrlEncodedContent(contentWithPrices)

// Place OTO order
let placeOTOOrder
    (apiToken: string)
    (accountId: string)
    (oto: OTOOrder)
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

        let content = buildOTOFormContent accountId oto duration

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

// Example usage - Buy stock then set take profit
let exampleOTOBuyWithTakeProfit () =
    let otoOrder =
        { PrimaryOrder =
            { Symbol = "AAPL"
              Side = Buy
              Quantity = 100
              Type = Market
              Price = None
              Stop = None }
          SecondaryOrder =
            { Symbol = "AAPL"
              Side = Sell
              Quantity = 100
              Type = Limit
              Price = Some 55.00m
              Stop = None } }

    let apiToken = "YOUR_API_TOKEN"
    let accountId = "YOUR_ACCOUNT_ID"

    async {
        let! result = placeOTOOrder apiToken accountId otoOrder Duration.GTC true

        match result with
        | Ok response -> printfn "OTO order placed successfully: %s" response
        | Error error -> printfn "Failed to place order: %s" error
    }
    |> Async.RunSynchronously

// Example usage - Buy stock then set stop loss
let exampleOTOBuyWithStopLoss () =
    let otoOrder =
        { PrimaryOrder =
            { Symbol = "TSLA"
              Side = Buy
              Quantity = 50
              Type = Limit
              Price = Some 200.00m
              Stop = None }
          SecondaryOrder =
            { Symbol = "TSLA"
              Side = Sell
              Quantity = 50
              Type = Stop
              Price = None
              Stop = Some 190.00m } }

    let apiToken = "YOUR_API_TOKEN"
    let accountId = "YOUR_ACCOUNT_ID"

    async {
        let! result = placeOTOOrder apiToken accountId otoOrder Duration.GTC true

        match result with
        | Ok response -> printfn "OTO order placed successfully: %s" response
        | Error error -> printfn "Failed to place order: %s" error
    }
    |> Async.RunSynchronously

// Example usage - Options strategy
let exampleOTOOptionsStrategy () =
    let otoOrder =
        { PrimaryOrder =
            { Symbol = "AAPL250117C00150000" // AAPL Jan 17 2025 $150 Call
              Side = BuyToOpen
              Quantity = 10
              Type = Limit
              Price = Some 5.50m
              Stop = None }
          SecondaryOrder =
            { Symbol = "AAPL250117C00150000"
              Side = SellToClose
              Quantity = 10
              Type = Limit
              Price = Some 7.50m // Take profit at 36% gain
              Stop = None } }

    let apiToken = "YOUR_API_TOKEN"
    let accountId = "YOUR_ACCOUNT_ID"

    async {
        let! result = placeOTOOrder apiToken accountId otoOrder Duration.GTC true

        match result with
        | Ok response -> printfn "OTO options order placed successfully: %s" response
        | Error error -> printfn "Failed to place order: %s" error
    }
    |> Async.RunSynchronously
