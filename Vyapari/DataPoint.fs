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

                member this.Init() = DataPoint.Init() }
 
        interface Buffer<DataPoint> with
            member this.Initialize() = Init()
            member this.BufferQueue(insert): BufferQueue<DataPoint> =
                LinearBuffer.Queue(adapter, insert)
