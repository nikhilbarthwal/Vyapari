namespace Vyapari.Tests

open NUnit.Framework
open Vyapari


module Buffer =

    let private interval: time = 10
    let private buckets = 5
    let private tag = "Buffer Test"


    let private genBars: DataPoint list =
        let random = System.Random(System.Guid.NewGuid().GetHashCode())

        let init =
            let x = int64 <| 100000.0 * (1.0 + random.NextDouble()) in x - (x % interval)

        let batch (previous: time, times: time list list) (n:int) =
            let pos = int64 n
            let start = previous + pos
            let stop = if n = buckets then start else (previous + interval - 1L)
            previous + interval * (pos + 1L), [start .. stop]::times

        let timelines = [0 .. buckets] |> List.fold batch (init, []) |> snd |> List.rev

        timelines |> List.concat |> List.map (Utils.Bar random)

    let private batch t = (t - t % interval) / interval

    let private addMap (m: Map<time, DataPoint list>) (b: DataPoint) =
        let key = batch b.Time
        if m.ContainsKey(key) then m.Add(key, b::m[key]) else m.Add(key, [b])

    let private check(f1: decimal, f2: decimal): bool = abs(f1 - f2) < 0.005m

    type private combine(input: DataPoint list) =
        let data: Map<time, DataPoint list> = List.fold addMap Map.empty input
        member this.Combine(b: DataPoint): bool =
            let bars: DataPoint list = data[batch b.Time]
            check(bars |> List.map _.Ask |> List.average, b.Ask) &&
            check(bars |> List.map _.Bid |> List.average, b.Bid)

    let private placement (first: DataPoint) (last: DataPoint) (z: DataPoint): bool =
        let d0 = decimal <| (batch z.Time) - (batch first.Time)
        let d1 = decimal <| (batch last.Time) - (batch first.Time)

        let decide xFirst xLast x=
            check(d1 * (x - xFirst) , d0 * (xLast - xFirst))

        let result = (decide first.Ask last.Ask z.Ask) &&
                     (decide first.Bid last.Bid z.Bid)
        if result then true else
            Log.Warning(tag, $"Placement failed at {z}") ; false

    let private equal (x: DataPoint) (y: DataPoint): bool = (x.Time = y.Time)

    type private BufferTest(buffer: Data.Buffer<DataPoint>) =
        let mutable insertMap: Map<time, DataPoint list> = Map.empty
        let mutable resetMap: Map<time, DataPoint list> = Map.empty
        let resetData (bar: DataPoint): unit = Log.Info(tag, $"Reset Bar -> {bar}")
                                               resetMap <- addMap resetMap bar
        let insertData (bar: DataPoint): unit = Log.Info(tag, $"Inserting -> {bar}")
                                                assert (bar.Time % interval = 0L)
                                                insertMap <- addMap insertMap bar

        let verifyKey (m: Map<time, DataPoint list>, b: DataPoint): bool =
            let key = batch b.Time
            if m.ContainsKey(key) then m[batch b.Time].Length = 1 else true

        let queue = buffer.Queue(insertData)

        member this.Append(sample: DataPoint): bool =
            if queue.Ingest(sample) then
                verifyKey (insertMap, sample)
            else
                resetData sample
                verifyKey (resetMap, sample)

        member this.GetInsert(key: time): DataPoint =
            assert (insertMap[key].Length = 1) ; insertMap[key].Head

        member this.NotInInsert(key: time): bool = not <| insertMap.ContainsKey(key)

        member this.GetReset(key: time): DataPoint =
            assert (resetMap[key].Length = 1) ; resetMap[key].Head

    let private verify (samples: DataPoint list): bool =

        let buffer = BufferTest <| DataPoint.Buffer(interval, buckets)
        let combine = combine(samples)

        let check (x: DataPoint) (current: time) (previous: time): bool =
            match (int <| current - previous) with
            | 0 -> true
            | gap when gap > buckets -> equal x <| buffer.GetReset(current)
            | _ -> if buffer.NotInInsert(previous) then true else
                       let prev = buffer.GetInsert(previous)
                       if combine.Combine(prev) then
                           [previous + 1L .. current - 1L]
                           |> List.map buffer.GetInsert
                           |> Utils.Test (placement prev x)
                       else Log.Warning(tag, $"Combine failed at {x}") ; false

        let rec fold: time * DataPoint list -> bool = function
            | previous, h::t ->
                if (buffer.Append h) then
                    let current = batch h.Time
                    if (check h current previous) then fold(current, t) else false
                else Log.Warning(tag, $"Duplicate Buffer key at {h.Time}") ; false
            | _ -> true

        buffer.Append samples.Head |> ignore
        let init = (batch <| samples.Head.Time) in fold (init, samples.Tail)


    [<Test>]
    let Buffer() = Assert.That(verify <| genBars)
