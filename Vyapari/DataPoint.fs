namespace Vyapari


[<Struct>]
type DataPoint (ask: float, bid: float, time: time, volume: int64) =
    member this.Ask = assert (ask >= bid) ; Utils.Normalize(ask)
    member this.Bid = assert (ask >= bid) ; Utils.Normalize(bid)
    member this.Time = time
    member this.Timestamp = Utils.ToDateTime(time)
    member this.Price = (this.Ask + this.Bid) / 2.0m
    member this.Volume = volume
    override this.ToString() =
        let ts = Utils.Ascii <| this.Timestamp.ToString("F")
        let bid = this.Bid in let ask = this.Ask
        $"Ask: {ask} / Bid: {bid} / Timestamp: {ts} / Epoch: {this.Time}"

    interface Data<DataPoint> with
        member this.Price = this.Price
        member this.Time = this.Time


module DataPoint =

    let Init() = DataPoint (0.0, 0.0, 0L, 0L)

    let private merge (r1: int) (d1: DataPoint) (r2: int) (d2: DataPoint) =
        let ask = LinearBuffer.BisectFloat r1 d1.Ask r2 d2.Ask
        let bid = LinearBuffer.BisectFloat r1 d1.Bid r2 d2.Bid
        let time = LinearBuffer.BisectLong r1 d1.Time r2 d2.Time
        let volume = LinearBuffer.BisectLong r1 d1.Volume r2 d2.Volume
        DataPoint(ask, bid, time, volume)

    type Buffer(interval, bucketCount) =
        interface Buffer<DataPoint> with

            member this.BufferQueue(insert): BufferQueue<DataPoint> =
                Buffer.Queue(insert, bucketCount, interval, Init, merge, extrapolate)
