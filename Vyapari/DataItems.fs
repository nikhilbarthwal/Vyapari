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

    let Array(length: int) = Data.Array<DataPoint>(length, DataPoint.Init)

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
 
        interface Data.Buffer<DataPoint> with
            member this.Init() = DataPoint.Init()
            member this.Queue(insert): Data.BufferQueue<DataPoint> =
                LinearBuffer.Queue(adapter, insert)

(*
    [<Struct>]
    type Bar (o: float, h: float, l: float, c: float, time: time, volume: int64) =
        member this.Open = Utils.Normalize(o)
        member this.High = assert (h >= l) ; Utils.Normalize(h)
        member this.Low = assert (h >= l) ; Utils.Normalize(l)
        member this.Close = Utils.Normalize(c)
        member this.Time = time
        member this.Timestamp = Utils.ToDateTime(time)
        member this.Price = Utils.Normalize((this.Open + this.Close) / 2.0)
        member this.Volume = volume
        override this.ToString() =
            let ts = Utils.Ascii <| this.Timestamp.ToString("F")
            $"Open: {this.Open} / High: {this.High} / Low: {this.Low} / Close: " +
            $"{this.Close} / Timestamp: {ts} / Epoch: {this.Time}"

        static member Init() = Bar (0.0, 0.0, 0.0, 0.0, 0L, 0L)

        interface Data<Bar> with
           member this.Price = this.Price
           member this.Time = this.Time
           member this.Merge(ratio1: int, ratio2:int, b: Bar) =
                Bar(o = (Utils.BisectFloat ratio1 ratio2 o b.Open),
                    h = (Utils.BisectFloat ratio1 ratio2 h b.High),
                    l = (Utils.BisectFloat ratio1 ratio2 l b.Low),
                    c = (Utils.BisectFloat ratio1 ratio2 c b.Close),
                    time = (Utils.BisectLong ratio1 ratio2 time b.Time),
                    volume = (Utils.BisectLong ratio1 ratio2 time b.Volume))

*)