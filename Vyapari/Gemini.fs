namespace Vyapari


open System.Text.Json

module Gemini =

    type private Parser(symbol: string, delta: decimal, insert: DataPoint -> unit) =

        let tag: string = $"Gemini #{symbol}"
        let mutable bestAsk: Maybe<decimal> = No
        let mutable bestBid: Maybe<decimal> = No

        let processEvent (event: JsonElement): unit =
            if (event.GetProperty("type").GetString() = "change" &&
                event.GetProperty("reason").GetString() = "place") then

                let price = decimal <| event.GetProperty("price").GetString()
                let side: string = event.GetProperty("side").GetString()

                if side = "ask" then
                    match bestAsk with
                    | Yes(ask) -> if price < ask then bestAsk <- Yes(price)
                    | No -> bestAsk <- Yes(price)

                if side = "bid" then
                    match bestBid with
                    | Yes(bid) -> if price > bid then bestBid <- Yes(price)
                    | No -> bestBid <- Yes(price)

        let processMessage (json: JsonElement): unit =
            if json.GetProperty("socket_sequence").GetInt64() > 0 then
                for k in [1 .. json.GetProperty("events").GetArrayLength()] do
                    processEvent <| json.GetProperty("events").Item(k - 1)

        let insert (ask: decimal) (bid: decimal) (json: JsonElement): unit =
            let point ask bid : DataPoint =
                let t = json.GetProperty("timestamp").GetInt64()
                { Ask = ask ; Bid = bid ; Time = t ; Volume = -1L }

            if bid >= ask then (insert <| point bid bid) else
                if ((100.0m * (ask - bid)) / bid) < delta then
                    insert <| point ask bid

        let parse(message: string): unit =
            let json: JsonElement = JsonDocument.Parse(message).RootElement
            processMessage json
            match bestAsk, bestBid with
            | Yes(ask), Yes(bid) -> insert ask bid json
            | _ -> ()

        interface Socket.Config with

            member this.Url: string =
                $"wss://api.gemini.com/v1/marketdata/{symbol}USD"

            member this.Initialize _ = ()
            member this.Tag: string = tag
            member this.Close _ = ()
            member this.Receiver(message: string, _): unit =
                try (parse message) with e ->
                    Log.Error(tag, $"Unable to parse {message} -> {e.Message}")


    type Connection(tickers: Ticker list,
                    length: int,
                    buffer: Data.Buffer<DataPoint>,
                    AskBidDelta: decimal) =

        let data = Data.Map(tickers, length, buffer)
        let store: Data.Store<DataPoint> = data

        let parser (ticker: Ticker): Parser option =
            match ticker with
            | Crypto(symbol) when (symbol = "BTC" || symbol = "ETH") ->
                Some <| Parser(symbol, AskBidDelta, store[ticker].Insert)
            | x ->
                Log.Warning("Gemini", $"Gemini does not support {x}") ; None

        let connection parser: System.IDisposable = new Socket.Connection(parser)
        let parsers = List.choose parser tickers
        let connections = Utils.CreateDictionary(parsers, connection)

        member this.DataSource: Data.Source<DataPoint> = data

        interface System.IDisposable with
            member this.Dispose() = for c in connections.Values do c.Dispose()
