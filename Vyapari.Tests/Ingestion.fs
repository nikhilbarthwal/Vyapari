namespace Vyapari.Tests

open System.Collections.Generic
open NUnit.Framework
open Vyapari


module Ingestion =

    let private interval(): time = 10
    let private size() = 100
    let private buckets() = 5
    let private tag() = "Ingestion Test"

    let private tickers() = [ Stock("A") ; Stock("B") ; Stock("C") ; Stock("D")
                              Stock("E") ; Stock("F") ; Stock("G") ; Stock("H") ]

    let private genDataPoints(tickers: Ticker list, interval: time, length: int):
            (Ticker * DataPoint) list =

        let random = System.Random(System.Guid.NewGuid().GetHashCode())
        let init = int64 <| 100000.0 * (1.0 + random.NextDouble())

        let gen (state: time list) (_: int): time list =
            let t = random.NextInt64(interval + 1L) in (state.Head + t)::state

        let timeline (s:Ticker): (Ticker * DataPoint) list =
            List.fold gen [init] [1 .. (int <| interval) * length]
            |> List.map (Utils.Bar random) |> List.map (fun b -> (s,b))

        tickers |> List.collect timeline |> List.sortBy (fun (_, b) -> b.Time)

    let private verify (source: Data.Source<DataPoint>) (tag: string)
                       (interval: time) (length: int) (ticker: Ticker): bool =
        let check (x: IReadOnlyList<DataPoint>) (i: int): bool =
            let diff: time = x[i-1].Time - x[i].Time
            if diff = interval then true else
                let m = $"Diff = {diff} <> interval = {interval} at {i} for {ticker}"
                Log.Warning(tag,  m) ; false

        let prices = DataPoint.Array(length)
        if source[ticker].Get(prices) then
            if List.forall (check prices) [1 .. length - 1] then
                Log.Info(tag, $"Ingestion successfully passed for {ticker}") ; true
            else
                Log.Info(tag, $"Ingestion failed for {ticker}") ; false
        else Log.Warning(tag, $"Failed to fetch data for {ticker}") ; false


    [<Test>]
    let Ingestion() =
        let tag, interval, size, tickers = tag(), interval(), size(), tickers()
        let buffer = DataPoint.Buffer(interval, buckets(), Data.Buffer.All)
        let data = DataPoint.Map(tickers, size, buffer)
        let store: Data.Store<DataPoint> = data
        for ticker, bar in genDataPoints(tickers, interval, size) do
            store[ticker].Insert bar
        Assert.That(Utils.Test (verify data tag interval size) tickers)
