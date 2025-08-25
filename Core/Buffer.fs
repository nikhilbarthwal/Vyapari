namespace Vyapari.Core

open Vyapari


module Buffer =

    type private Bucket<'T>(init: unit -> 'T, merge: int -> 'T -> 'T -> 'T) =
        let mutable data: 'T = init()
        let mutable count = 0

        member this.Data = assert (count > 0) ; data
        member this.Reset() = data <- init() ; count <- 0
        member this.Count = count
        member this.Add(x: 'T) =
            if count > 0 then (data <- merge count data x) else (data <- x)
            count <- count + 1

    type private Buckets<'T when 'T :> Data<'T>>(bucketCount: int,
                                                 init: unit -> 'T,
                                                 merge: int -> 'T -> 'T -> 'T) =
        let mutable pos = 0
        let buckets = Array.Initialize(bucketCount, fun _ -> Bucket(init, merge))
        let index k = (pos + k) % bucketCount
        do assert (bucketCount > 1)

        member this.Item with get(k: int) = buckets[index(k)]
        member this.Previous() = buckets[pos].Data.Time
        member this.Shift(k) =
            assert (k > 0)
            for i in [1 .. k] do buckets[index <| i-1].Reset()
            pos <- (pos + k) % bucketCount

        member this.Reset() =
            pos <- 0 ; for i in [0 .. bucketCount - 1] do buckets[i].Reset()

    type internal Queue<'T when 'T :> Data<'T>>(
        output: 'T -> unit,
        bucketCount: int,
        interval: time,
        init: unit -> 'T,
        merge: int -> 'T -> 'T -> 'T,
        extrapolate: 'T -> 'T -> int -> time -> time -> int -> 'T) =

        let buckets = Buckets(bucketCount, init, merge)
        let mutable previous: time = 0L
        let floor (t:time) = t - (t % (int64 interval))

        let extrapolate (diff: int): unit =
#if DEBUG
            assert (diff > 0)
            assert (buckets[0].Count > 0)
            assert (buckets[diff].Count = 1)
            if diff > 1 then
                for k in [1 .. diff - 1] do assert(buckets[k].Count = 0)
#endif
            let eval = extrapolate (buckets[diff].Data) (buckets[0].Data)
                                   diff previous interval
            for k in [0 .. diff - 1] do (output <| eval k)

        interface BufferQueue<'T> with
            member this.Ingest(input: 'T): bool =
                let current = floor input.Time
                if buckets[0].Count = 0 then // Initial case
                    buckets[0].Add input
                    previous <- current ; true
                else
                    assert (input.Time >= buckets.Previous())
                    let diff = int <| (current - previous) / interval
                    if diff >= bucketCount then
                        buckets.Reset()
                        buckets[0].Add input
                        previous <- current ; false
                    else
                        buckets[diff].Add input
                        if diff > 0 then
                            extrapolate diff
                            buckets.Shift(diff)
                        previous <- current ; true
