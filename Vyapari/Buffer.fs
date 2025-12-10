namespace Vyapari

open Vyapari


module LinearBuffer =
    
    type internal Adapter<'T> =
        abstract BucketCount: int
        abstract Interval: time
        abstract Merge<'T>: int -> int -> 'T -> 'T -> time -> 'T
        abstract Init<'T>: unit -> 'T

    
    module Bisect =
        let inline internal Decimal (r1: int) (r2: int)
                                    (v1: decimal) (v2: decimal) =
            (v1 * (decimal r1) + v2 * (decimal r2)) / (decimal <| r1 + r2)

        let inline internal Long (r1: int) (r2: int) (v1: int64) (v2: int64) =
            (v1 * (int64 r1) + v2 * (int64 r2)) / (int64 <| r1 + r2)


    type private Bucket<'T when 'T :> Data<'T>>(adapter: Adapter<'T>) =
        let mutable data: 'T = adapter.Init()
        let mutable count = 0

        member this.Data = assert (count > 0) ; data
        member this.Reset() = data <- adapter.Init() ; count <- 0
        member this.Count = count
        member this.Add(x: 'T) (t: time) =
            if count > 0 then
                data <- adapter.Merge 1 count x data t
            else (data <- x)
            count <- count + 1
        static member Create(adapter: Adapter<'T>) (_:int) = Bucket(adapter)

    type private Buckets<'T when 'T :> Data<'T>>(adapter: Adapter<'T>) =
        let mutable pos = 0
        let buckets = Array.Initialize(adapter.BucketCount, Bucket.Create(adapter))
        let index k = (pos + k) % adapter.BucketCount
        do assert (adapter.BucketCount > 1)

        member this.Item with get(k: int) = buckets[index(k)]
        member this.Previous() = buckets[pos].Data.Time
        member this.Shift(k) =
            assert (k > 0)
            for i in [1 .. k] do buckets[index <| i-1].Reset()
            pos <- (pos + k) % adapter.BucketCount

        member this.Reset() =
            pos <- 0 ; for i in [0 .. adapter.BucketCount - 1] do buckets[i].Reset()

    type internal Queue<'T when 'T :> Data<'T>>(adapter: Adapter<'T>,
                                                output: 'T -> unit) =

        let buckets = Buckets(adapter)
        let mutable previous: time = 0L
        let floor (t:time) = t - (t % (int64 adapter.Interval))

        let extrapolate (diff: int): unit =
#if DEBUG
            assert (diff > 0)
            assert (buckets[diff].Count = 1)
            assert(buckets[0].Count > 0)
            if diff > 1 then
                for k in [1 .. diff - 1] do assert(buckets[k].Count = 0)
#endif
            for k in [0 .. diff - 1] do
                previous + adapter.Interval * (int64 k)
                |> adapter.Merge k (diff - k) (buckets[diff].Data) (buckets[0].Data)
                |> output

        interface BufferQueue<'T> with
            member this.Ingest(input: 'T): bool =
                let current = floor input.Time
                if buckets[0].Count = 0 then // Initial case
                    buckets[0].Add input current
                    previous <- current ; true
                else
                    assert (input.Time >= buckets.Previous())
                    let diff = int <| (current - previous) / adapter.Interval
                    if diff >= adapter.BucketCount then
                        buckets.Reset()
                        buckets[0].Add input current
                        previous <- current ; false
                    else
                        buckets[diff].Add input current
                        if diff > 0 then
                            extrapolate diff
                            buckets.Shift(diff)
                        previous <- current ; true
