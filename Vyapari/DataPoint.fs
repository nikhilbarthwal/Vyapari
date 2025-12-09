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
    let Prices(length: int) = Data.Price<DataPoint>(length, Init)

    let private merge (count: int) (data: DataPoint) (x: DataPoint) =
        let avgFloat = Utils.BisectFloat count 1
        let avgLong = Utils.BisectLong count 1
        { Ask = avgFloat data.Ask x.Ask
          Bid = avgFloat data.Bid x.Bid
          Time = avgLong data.Time x.Time
          Volume = avgLong data.Volume x.Volume }

    let private extrapolate (curr: DataPoint) (prev: DataPoint) (diff: int)
                    (previous: time) (interval: time) (k: int) =
        let extrapolateFloat = Utils.BisectFloat k <| diff - k
        let extrapolateLong = Utils.BisectLong k <| diff - k
        { Ask = extrapolateFloat curr.Ask prev.Ask
          Bid = extrapolateFloat curr.Bid prev.Bid
          Time = previous + interval * (int64 k)
          Volume = extrapolateLong curr.Volume prev.Volume }

    type Buffer(interval, bucketCount) =
        interface Buffer<DataPoint> with
            member this.Initialize() = Init()
            member this.BufferQueue(insert): BufferQueue<DataPoint> =
                Buffer.Queue(insert, bucketCount, interval, Init, merge, extrapolate)
