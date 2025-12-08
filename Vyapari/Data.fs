namespace Vyapari


type Data<'T when 'T :> Data<'T>> = abstract member Price: float
                                    abstract member Time: time

and BufferQueue<'T when 'T :> Data<'T>> = abstract member Ingest: 'T -> bool

and Buffer<'T when 'T :> Data<'T>> =
    abstract member BufferQueue: ('T -> unit) -> BufferQueue<'T>
    abstract member Initialize: unit -> 'T


module Data =

    type Price<'T when 'T :> Data<'T>> internal(length: int, f: unit -> 'T) =
        let data = Array.Buffer(length, fun _ -> f())
        member internal this.Update(get: int -> 'T) () = data.Overwrite(get)
        member this.Data: Array<'T> = data

    type Array<'T when 'T :> Data<'T>> = abstract Get: Price<'T> -> bool


    type private RingBuffer<'T when 'T :> Data<'T>>(ticker: Ticker,
                                                     length: int,
                                                     buffer: Buffer<'T>,
                                                     verbose: bool) =
        let mutable pos: int = 0
        let mutable count: int = 0

        let data = Array.Buffer(length, fun _ -> buffer.Initialize())
        let get i = let k = (length + pos - i - 1) % length in data[k]

        let insert(x: 'T) () = data[pos] <- x ; count <- count + 1
                               pos <- pos + 1 ; if pos = length then pos <- 0

        let reset() = count <- 0 ; pos <- 0

        let queue = buffer.BufferQueue(insert)

        member internal this.Reset() =
            if verbose then Log.Info("Data", $"Reset for {ticker}")
            lock object reset

        member internal this.Insert(x: 'T) =
            if (not <| queue.Ingest(x)) then this.Reset()

        interface Array<'T> with
            member this.Get(prices: Price<'T>): bool =
                if count < length then false else
                    (prices.Update get) ; true


    type Source<'T when 'T :> Data<'T>> =
        abstract Tickers: Ticker list
        abstract Item: Ticker -> Array<'T> with get
        abstract BufferLength: int


    type Store<'T when 'T :> Data<'T>>(tickers: Ticker list,
                                       length: int,
                                       buffer: Buffer<'T>,
                                       verbose: bool) =
        let store ticker = ArrayBuffer(ticker, length, buffer, verbose)
        let dataMap = Utils.CreateDictionary(tickers, store)
        member this.Tickers: Ticker list = tickers
        member this.Reset ticker = dataMap[ticker].Reset()
        member this.Insert ticker data = dataMap[ticker].Insert(data)

        interface Source<'T> with
            member this.Tickers: Ticker list = tickers
            member this.BufferLength: int = length
            member this.Item with get(ticker: Ticker) = dataMap[ticker]
