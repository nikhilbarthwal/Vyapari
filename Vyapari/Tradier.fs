namespace Vyapari


open System.Net
open System.Text.Json
open System.Net.Http

module Tradier =

    type private Adapter(tag: string, store: Data.Store<DataPoint>, token: string) =


        let wsUrl = "wss://ws.tradier.com/v1/markets/events"

        let ticker2symbol (m: Map<string, Ticker>) (ticker: Ticker) =
            match ticker with
            | Stock(symbol) ->
                m.Add(symbol, ticker)
            | Crypto(symbol) ->
                Log.Warning(tag, $"Crypto {symbol} not supported in {tag}") ; m
            | Option(symbol, strike, expiry, ty) ->
                let d = expiry.ToString("yyMMdd")
                let p: int = int <| strike * 1000.0
                let symbol = $"{symbol}{d}{ty.ToString().Substring(0, 1)}%08d{p}"
                m.Add(symbol, ticker)

        let tickers = store.Tickers |> List.fold ticker2symbol Map.empty

        let payload(): string =
            try
                let url = "https://api.tradier.com/v1/markets/events/session"
                let tickers = "\"" + (tickers.Keys |> String.concat "\", \"") + "\""
                Log.Info(tag, "Attempting to log into Tradier")
                use client = new HttpClient()
                let auth = Headers.AuthenticationHeaderValue("Bearer", token)
                client.DefaultRequestHeaders.Authorization <- auth
                let json = new StringContent("{}", System.Text.Encoding.UTF8)
                let resp = client.PostAsync(url, json).GetAwaiter().GetResult()
                let text = resp.Content.ReadAsStringAsync().GetAwaiter().GetResult()
                if resp.StatusCode = HttpStatusCode.OK then
                    let xmlDoc: System.Xml.XmlDocument  = System.Xml.XmlDocument()
                    xmlDoc.LoadXml(text)
                    let session = xmlDoc.FirstChild.ChildNodes[1].InnerText
                    let currentTime = Utils.CurrentTime()
                    Log.Info(tag, $"Starting session Id: {session} at {currentTime}")
                    let msg = $"[{tickers}], \"sessionid\": \"{session}\" ,"
                    "{\"symbols\": " + msg + "\"linebreak\": false}"
                else Log.Error(tag, $"Failed to get Session Id, Response: {text}")
            with
            | ex -> Log.Exception(tag, "Failed to get Session Id", ex)

        let receiver: string -> unit = function
            | "Initial" -> Log.Info(tag, "Initial message received")
            | "NoMessageReceived" -> Log.Info(tag, "No message received")
            | msg ->
                try
                    let json: JsonElement = JsonDocument.Parse(msg).RootElement
                    if json.GetProperty("type").GetString() = "quote" then
                        let timestamp = json.GetProperty("askdate").GetString()
                        let epoch: time = System.Int64.Parse(timestamp) / 1000L
                        let symbol = json.GetProperty("symbol").GetString()
                        let input: Data.Input<DataPoint> = store[tickers[symbol]]
                        let get (key: string) = Utils.Normalize <| json.GetProperty(key).GetDouble()
                        input.Insert({ Ask = get "ask" ; Bid = get "bid"
                                       Time = epoch ; Volume = -1L })
                with ex ->
                    Log.Warning(tag,
                        $"Unable to parse message {msg}, Exception: {ex.Message}")

        interface Socket.Config with
            member this.Url = wsUrl
            member this.Initialize(send) = payload() |> send
            member this.Receiver(msg, _) = receiver msg
            member this.Tag: string = tag
            member this.Close _ = ()

    type Client(tickers: Ticker list,
                length: int,
                buffer: Data.Buffer<DataPoint>,
                token: string) =

            let tag = "Tradier"
            let dataStore = Data.Map(tickers, length, buffer)
            let config = Adapter(tag, dataStore, token)
            let connection = new Socket.Connection(config)

            interface Client<DataPoint> with
                member this.DataSource: Data.Source<DataPoint> = dataStore
                member this.Dispose() =
                    let x: System.IDisposable = connection in x.Dispose()
