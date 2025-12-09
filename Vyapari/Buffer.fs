namespace Vyapari

open Vyapari


module LinearBuffer =

    type internal Bisect<'T when 'T :> Data<'T>> = int -> 'T -> int -> 'T -> 'T

    let inline internal BisectDecimal (r1: int) (v1: decimal)
                                      (r2: int) (v2: decimal) =
        (v1 * (decimal r1) + v2 * (decimal r2)) / (decimal <| r1 + r2)

    let inline internal BisectLong (r1: int) (v1: int64) (r2: int) (v2: int64) =
        (v1 * (int64 r1) + v2 * (int64 r2)) / (int64 <| r1 + r2)

    type private Bucket<'T when 'T :> Data<'T>>(merge: Bisect<'T>) =
        let mutable data: 'T = 'T.Init()
        let mutable count = 0

        member this.Data = assert (count > 0) ; data
        member this.Reset() = data <- 'T.Init() ; count <- 0
        member this.Count = count
        member this.Add(x: 'T) =
            if count > 0 then (data <- merge 1 x count data) else (data <- x)
            count <- count + 1

    type private Buckets<'T when 'T :> Data<'T>>(bucketCount: int,
                                                 merge: Bisect<'T>) =
        let mutable pos = 0
        let buckets = Array.Initialize(bucketCount, fun _ -> Bucket(merge))
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
        merge: Bisect<'T>) =

        let buckets = Buckets(bucketCount, merge)
        let mutable previous: time = 0L
        let floor (t:time) = t - (t % (int64 interval))

        let extrapolate (diff: int): unit =
#if DEBUG
            assert (diff > 0)
            assert (buckets[diff].Count = 1)
            for k in [0 .. diff - 1] do assert(buckets[k].Count = 0)
#endif
            for k in [0 .. diff - 1] do
                output <| merge k (buckets[0].Data) (diff - k) (buckets[diff].Data)

        interface Data.BufferQueue<'T> with
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
