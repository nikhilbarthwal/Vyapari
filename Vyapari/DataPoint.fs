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
        static member Init() = Data.Init()


module DataPoint =

    let private merge (r1: int) (d1: DataPoint) (r2: int) (d2: DataPoint) = {
        Ask = LinearBuffer.BisectDecimal r1 d1.Ask r2 d2.Ask
        Bid = LinearBuffer.BisectDecimal r1 d1.Bid r2 d2.Bid
        Time = LinearBuffer.BisectLong r1 d1.Time r2 d2.Time
        Volume = LinearBuffer.BisectLong r1 d1.Volume r2 d2.Volume }

    type Buffer(interval, bucketCount) =
        interface Data.Buffer<DataPoint> with
            member this.Queue(insert): Data.BufferQueue<DataPoint> =
                LinearBuffer.Queue(insert, bucketCount, interval, merge)
