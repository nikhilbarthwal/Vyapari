namespace Vyapari


type Data<'T when 'T :> Data<'T>> = abstract member Price: float
                                    abstract member Time: time

and BufferQueue<'T when 'T :> Data<'T>> = abstract member Ingest: 'T -> bool

and Buffer<'T when 'T :> Data<'T>> =
    abstract member BufferQueue: ('T -> unit) -> BufferQueue<'T>
    abstract member Initialize: unit -> 'T


module Data =

    type StoreArray<'T when 'T :> Data<'T>> = abstract Get: Array.Buffer<'T> -> bool

    type private StoreArrayBuffer<'T when 'T :> Data<'T>>(ticker: Ticker,
                                                          length: int,
                                                          buffer: Buffer<'T>,
                                                          verbose: bool) =
        let mutable pos: int = 0
        let mutable count: int = 0

        let object = System.Object()
        let data = Array.Buffer(length, fun _ -> buffer.Initialize())
        let get i = let k = (length + pos - i - 1) % length in data[k]

        let insert(x: 'T) () = data[pos] <- x ; count <- count + 1
                               pos <- pos + 1 ; if pos = length then pos <- 0

        let reset() = count <- 0 ; pos <- 0

        let update (input: Array.Buffer<'T>) () = input.Overwrite(get)

        let ingest x =
            if verbose then Log.Info("Data", $"Price for {ticker} -> {x}")
            lock object (insert x)

        let queue = buffer.BufferQueue(ingest)

        member this.Reset() =
            if verbose then Log.Info("Data", $"Reset for {ticker}")
            lock object reset

        member this.Insert(x: 'T) =
            if (not <| queue.Ingest(x)) then this.Reset()

        interface StoreArray<'T> with
            member this.Get(input: Array.Buffer<'T>): bool =
                if count < length then false else
                    lock object (update input) ; true


    type Source<'T when 'T :> Data<'T>> =
        abstract Tickers: Ticker list
        abstract Item: Ticker -> StoreArray<'T> with get
        abstract BufferLength: int


    type Store<'T when 'T :> Data<'T>>(tickers: Ticker list,
                                       length: int,
                                       buffer: Buffer<'T>,
                                       verbose: bool) =
        let store ticker = StoreArrayBuffer(ticker, length, buffer, verbose)
        let dataMap = Utils.CreateDictionary(tickers, store)
        member this.Tickers: Ticker list = tickers
        member this.Reset(ticker: Ticker) = dataMap[ticker].Reset()
        member this.Insert (ticker: Ticker) (data: 'T) = dataMap[ticker].Insert(data)

        interface Source<'T> with
            member this.Tickers: Ticker list = tickers
            member this.BufferLength: int = length
            member this.Item with get(ticker: Ticker) = dataMap[ticker]
