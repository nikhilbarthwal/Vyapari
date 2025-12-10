module TradierBalance

open System
open System.Net.Http
open System.Text.Json

// Balance types
type AccountBalance =
    { OptionShortValue: decimal
      TotalEquity: decimal
      AccountNumber: string
      AccountType: string
      Closepl: decimal
      CurrentRequirement: decimal
      Equity: decimal
      LongMarketValue: decimal
      MarketValue: decimal
      Openpl: decimal
      OptionLongValue: decimal
      OptionRequirement: decimal
      PendingOrdersCount: int
      ShortMarketValue: decimal
      StockLongValue: decimal
      TotalCash: decimal
      UnclearedFunds: decimal
      PendingCash: decimal
      Margin: MarginInfo option }

and MarginInfo =
    { FedCall: decimal
      MaintenanceCall: decimal
      OptionBuyingPower: decimal
      StockBuyingPower: decimal
      StockShortValue: decimal
      Sweep: decimal }

// JSON deserialization helpers
let parseBalance (json: string) : Result<AccountBalance, string> =
    try
        use doc = JsonDocument.Parse(json)
        let root = doc.RootElement
        let balances = root.GetProperty("balances")

        let getDecimal (element: JsonElement) (property: string) =
            if element.TryGetProperty(property) |> fst then
                let prop = element.GetProperty(property)

                if prop.ValueKind = JsonValueKind.Number then
                    prop.GetDecimal()
                else
                    0m
            else
                0m

        let getInt (element: JsonElement) (property: string) =
            if element.TryGetProperty(property) |> fst then
                let prop = element.GetProperty(property)

                if prop.ValueKind = JsonValueKind.Number then
                    prop.GetInt32()
                else
                    0
            else
                0

        let getString (element: JsonElement) (property: string) =
            if element.TryGetProperty(property) |> fst then
                element.GetProperty(property).GetString()
            else
                ""

        let margin =
            if balances.TryGetProperty("margin") |> fst then
                let m = balances.GetProperty("margin")

                Some
                    { FedCall = getDecimal m "fed_call"
                      MaintenanceCall = getDecimal m "maintenance_call"
                      OptionBuyingPower = getDecimal m "option_buying_power"
                      StockBuyingPower = getDecimal m "stock_buying_power"
                      StockShortValue = getDecimal m "stock_short_value"
                      Sweep = getDecimal m "sweep" }
            else
                None

        let balance =
            { OptionShortValue = getDecimal balances "option_short_value"
              TotalEquity = getDecimal balances "total_equity"
              AccountNumber = getString balances "account_number"
              AccountType = getString balances "account_type"
              Closepl = getDecimal balances "close_pl"
              CurrentRequirement = getDecimal balances "current_requirement"
              Equity = getDecimal balances "equity"
              LongMarketValue = getDecimal balances "long_market_value"
              MarketValue = getDecimal balances "market_value"
              Openpl = getDecimal balances "open_pl"
              OptionLongValue = getDecimal balances "option_long_value"
              OptionRequirement = getDecimal balances "option_requirement"
              PendingOrdersCount = getInt balances "pending_orders_count"
              ShortMarketValue = getDecimal balances "short_market_value"
              StockLongValue = getDecimal balances "stock_long_value"
              TotalCash = getDecimal balances "total_cash"
              UnclearedFunds = getDecimal balances "uncleared_funds"
              PendingCash = getDecimal balances "pending_cash"
              Margin = margin }

        Ok balance
    with ex ->
        Error(sprintf "Failed to parse balance: %s" ex.Message)

// Get account balance
let getAccountBalance (apiToken: string) (accountId: string) (sandbox: bool) =
    async {
        let baseUrl =
            if sandbox then
                "https://sandbox.tradier.com"
            else
                "https://api.tradier.com"

        let url = sprintf "%s/v1/accounts/%s/balances" baseUrl accountId

        use client = new HttpClient()

        client.DefaultRequestHeaders.Add(
            "Authorization",
            sprintf "Bearer %s" apiToken
        )

        client.DefaultRequestHeaders.Add("Accept", "application/json")

        try
            let! response = client.GetAsync(url) |> Async.AwaitTask

            let! responseBody =
                response.Content.ReadAsStringAsync() |> Async.AwaitTask

            if response.IsSuccessStatusCode then
                return parseBalance responseBody
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

// Get balances for all accounts
let getAllAccountBalances (apiToken: string) (sandbox: bool) =
    async {
        let baseUrl =
            if sandbox then
                "https://sandbox.tradier.com"
            else
                "https://api.tradier.com"

        let url = sprintf "%s/v1/user/balances" baseUrl

        use client = new HttpClient()

        client.DefaultRequestHeaders.Add(
            "Authorization",
            sprintf "Bearer %s" apiToken
        )

        client.DefaultRequestHeaders.Add("Accept", "application/json")

        try
            let! response = client.GetAsync(url) |> Async.AwaitTask

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

// Display balance information
let displayBalance (balance: AccountBalance) =
    printfn "=== Account Balance ==="
    printfn "Account Number: %s" balance.AccountNumber
    printfn "Account Type: %s" balance.AccountType
    printfn ""
    printfn "Total Equity: $%.2f" balance.TotalEquity
    printfn "Total Cash: $%.2f" balance.TotalCash
    printfn "Market Value: $%.2f" balance.MarketValue
    printfn ""
    printfn "--- Positions ---"
    printfn "Stock Long Value: $%.2f" balance.StockLongValue
    printfn "Option Long Value: $%.2f" balance.OptionLongValue
    printfn "Option Short Value: $%.2f" balance.OptionShortValue
    printfn "Short Market Value: $%.2f" balance.ShortMarketValue
    printfn ""
    printfn "--- P&L ---"
    printfn "Open P&L: $%.2f" balance.Openpl
    printfn "Close P&L: $%.2f" balance.Closepl
    printfn ""
    printfn "--- Other ---"
    printfn "Pending Orders: %d" balance.PendingOrdersCount
    printfn "Uncleared Funds: $%.2f" balance.UnclearedFunds
    printfn "Pending Cash: $%.2f" balance.PendingCash

    match balance.Margin with
    | Some margin ->
        printfn ""
        printfn "--- Margin Account ---"
        printfn "Stock Buying Power: $%.2f" margin.StockBuyingPower
        printfn "Option Buying Power: $%.2f" margin.OptionBuyingPower
        printfn "Fed Call: $%.2f" margin.FedCall
        printfn "Maintenance Call: $%.2f" margin.MaintenanceCall
    | None -> ()

    printfn "======================"

// Example usage - Get single account balance
let exampleGetBalance () =
    let apiToken = "YOUR_API_TOKEN"
    let accountId = "YOUR_ACCOUNT_ID"

    async {
        let! result = getAccountBalance apiToken accountId true

        match result with
        | Ok balance ->
            displayBalance balance

            // You can also access individual fields
            printfn ""
            printfn "Quick Summary:"
            printfn "Total Equity: $%.2f" balance.TotalEquity
            printfn "Cash Available: $%.2f" balance.TotalCash

            match balance.Margin with
            | Some m -> printfn "Buying Power: $%.2f" m.StockBuyingPower
            | None -> ()

        | Error error -> printfn "Failed to get balance: %s" error
    }
    |> Async.RunSynchronously

// Example usage - Get all account balances
let exampleGetAllBalances () =
    let apiToken = "YOUR_API_TOKEN"

    async {
        let! result = getAllAccountBalances apiToken true

        match result with
        | Ok json ->
            printfn "All account balances:"
            printfn "%s" json
        | Error error -> printfn "Failed to get balances: %s" error
    }
    |> Async.RunSynchronously

// Check if account has sufficient buying power
let hasSufficientBuyingPower (balance: AccountBalance) (requiredAmount: decimal) =
    match balance.Margin with
    | Some margin -> margin.StockBuyingPower >= requiredAmount
    | None -> balance.TotalCash >= requiredAmount
