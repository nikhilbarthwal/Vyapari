namespace Vyapari


[<Struct>]
type DataPoint = { Ask: decimal; Bid: decimal; Time: time; Volume: int64} with
    member this.Timestamp = Utils.ToDateTime(this.Time)
    member this.Price = (this.Ask + this.Bid) / 2m
    override this.ToString() =
         let ts = Utils.Ascii <| this.Timestamp.ToString("F")
         $"Ask: {this.Bid} / Bid: {this.Ask} / Timestamp: {ts} / Epoch: {this.Time}"

    static member Init() = { Ask = 0m ; Bid = 0m ; Time = 0L ; Volume = 0L }

    interface Data<DataPoint> with
        member this.Price = this.Price
        member this.Time = this.Time


module DataPoint =

    let Init() = { Ask = 0m ; Bid = 0m ; Time = 0L ; Volume = 0L }
    let Array(length: int) = Data.Price<DataPoint>(length, Init)

    type Buffer(interval: time, bucketCount: int) =
        let adapter: LinearBuffer.Adapter<DataPoint> =
            { new LinearBuffer.Adapter<DataPoint> with 
                member this.BucketCount = bucketCount
                member this.Interval = interval
                member this.Merge r1 r2 x1 x2  time =
                    let avgDecimal = LinearBuffer.Bisect.Decimal r1 r2
                    let avgLong = LinearBuffer.Bisect.Long r1 r2
                    { Ask = avgDecimal x1.Ask x2.Ask
                      Bid = avgDecimal x1.Bid x2.Bid
                      Time = time
                      Volume = avgLong x1.Volume x2.Volume }

                member this.Init() = DataPoint.Init()
(*
                member this.Update x time =
                    { Ask = x.Ask ; Bid = x.Bid ; Time = time ; Volume = x.Volume }
                    
                member this.Extrapolate (curr: DataPoint) (prev: DataPoint) (diff: int)
                                          (previous: time) (interval: time) (k: int) =
                    let extrapolateDecimal = LinearBuffer.Bisect.Decimal k <| diff - k
                    let extrapolateLong = LinearBuffer.Bisect.Long k <| diff - k
                    { Ask = extrapolateDecimal curr.Ask prev.Ask
                      Bid = extrapolateDecimal curr.Bid prev.Bid
                      Time = previous + interval * (int64 k)
                      Volume = extrapolateLong curr.Volume prev.Volume } *)
            }
        
 
        interface Buffer<DataPoint> with
            member this.Initialize() = Init()
            member this.BufferQueue(insert): BufferQueue<DataPoint> =
                LinearBuffer.Queue(adapter, insert)


(*
            let eval = adapter.Extrapolate (buckets[diff].Data) (buckets[0].Data)
                                            diff previous adapter.Interval

                                            k <| diff - k
            for k in [0 .. diff - 1] do
                let result = adapter.Merge k (diff - k) (buckets[0].Data) (buckets[diff].Data)
                let t = previous + adapter.Interval * (int64 k)
                let z = eval k
                assert (t = z.Time)
                *)