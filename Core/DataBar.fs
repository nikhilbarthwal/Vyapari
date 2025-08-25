    (* [<Struct>]
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
                    volume = (Utils.BisectLong ratio1 ratio2 time b.Volume)) *)
