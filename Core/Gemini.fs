namespace Vyapari.Core

open Vyapari
open System.Text.Json


module Gemini =

    type private Parser(symbol: string, difference: float,
                        timeout: int, ingest: DataPoint -> unit) =

        let tag: string = $"Gemini[{symbol}]"
        let mutable bestAsk: Maybe<float> = No
        let mutable bestBid: Maybe<float> = No

        let processEvent (event: JsonElement): unit =
            if (event.GetProperty("type").GetString() = "change" &&
                event.GetProperty("reason").GetString() = "place") then

                let price: float = float <| event.GetProperty("price").GetString()
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

        let insert (ask: float) (bid: float) (json: JsonElement): unit =
            let point ask bid : DataPoint =
                let t = json.GetProperty("timestamp").GetInt64()
                DataPoint (ask = ask, bid = bid, time = t, volume = -1L)

            if bid >= ask then (ingest <| point bid bid) else
                if ((100.0 * (ask - bid)) / bid) < difference then
                    ingest <| point ask bid

        let parse(message: string): unit =
            let json: JsonElement = JsonDocument.Parse(message).RootElement
            processMessage json
            match bestAsk, bestBid with
            | Yes(ask), Yes(bid) -> insert ask bid json
            | _ -> ()

        interface Socket.Adapter with
            member this.Timeout: int = timeout
            member this.Url: string =
                $"wss://api.gemini.com/v1/marketdata/{symbol}USD"

            member this.Initialize _ = ()
            member this.Tag: string = tag
            member this.Reconnect(msg, _) =
                Log.Warning(tag, $"Reconnecting for {symbol} -> {msg}")
            member this.Close _ = ()
            member this.Receive(message: string, _): unit =
                try (parse message) with e ->
                    Log.Error(tag, $"Unable to parse {message} -> {e.Message}")


    type Connection(tickers: Ticker list,
                    length: int,
                    buffer: Buffer<DataPoint>,
                    verbose: bool,
                    AskBidDifference: float,
                    Timeout: int) =

        let data = Data.Store(tickers, length, buffer, verbose)
        let connection (ticker: Ticker): Maybe<Socket.Connection> =
            match ticker with
            | Crypto(symbol) when (symbol = "BTC" || symbol = "ETH") ->
                Yes <| new Socket.Connection(
                    Parser(symbol, AskBidDifference, Timeout, data.Insert ticker))
            | x -> Log.Warning("Gemini", $"Gemini does not support {x}") ; No

        let connections = Utils.CreateDictionaryOpt(tickers, connection)
        member this.DataSource: Data.Source<DataPoint> = data
        interface System.IDisposable with
            member this.Dispose() =
                for conn in connections.Values do
                    let x: System.IDisposable = conn in x.Dispose()
