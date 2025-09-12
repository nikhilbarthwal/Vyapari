namespace Vyapari


[<Struct>]
type DataPoint (ask: float, bid: float, time: time, volume: int64) =
    member this.Ask = assert (ask >= bid) ; Utils.Normalize(ask)
    member this.Bid = assert (ask >= bid) ; Utils.Normalize(bid)
    member this.Time = time
    member this.Timestamp = Utils.ToDateTime(time)
    member this.Price = Utils.Normalize((this.Ask + this.Bid) / 2.0)
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

    let private merge (count: int) (data: DataPoint) (x: DataPoint) =
        let avgFloat = Utils.BisectFloat count 1
        let avgLong = Utils.BisectLong count 1
        DataPoint (ask   = avgFloat data.Ask x.Ask,
                   bid  = avgFloat data.Bid x.Bid,
                   time   = avgLong data.Time x.Time,
                   volume = avgLong data.Volume x.Volume)

    let private extrapolate (curr: DataPoint) (prev: DataPoint) (diff: int)
                    (previous: time) (interval: time) (k: int) =
        let extrapolateFloat = Utils.BisectFloat k <| diff - k
        let extrapolateLong = Utils.BisectLong k <| diff - k
        DataPoint(ask    = extrapolateFloat curr.Ask prev.Ask,
                  bid    = extrapolateFloat curr.Bid prev.Bid,
                  time   = previous + interval * (int64 k),
                  volume = extrapolateLong curr.Volume prev.Volume)

    type Buffer(interval, bucketCount) =
        interface Buffer<DataPoint> with
            member this.Initialize() = Init()
            member this.BufferQueue(insert): BufferQueue<DataPoint> =
                Buffer.Queue(insert, bucketCount, interval, Init, merge, extrapolate)
