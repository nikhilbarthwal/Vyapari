namespace Vyapari

#nowarn "3535"
open System.Collections


type Data<'T when 'T :> Data<'T>> = abstract member Price: decimal
                                    abstract member Time: time
                                    static abstract Init: unit -> 'T

module Data =

    type Array<'T when 'T :> Data<'T>> internal(length: int) =
        let data = Array.Buffer(length, fun _ -> 'T.Init())
        member internal this.Update(get: int -> 'T) = data.Overwrite(get)
        interface Vyapari.Array<'T> with
            member this.Item(index: int) = data[index]
            member this.Length = length
            member this.Get(index) = data.Get(index)

    type BufferQueue<'T when 'T :> Data<'T>> = abstract member Ingest: 'T -> bool

    type Buffer<'T when 'T :> Data<'T>> =
        abstract member Queue: ('T -> unit) -> BufferQueue<'T>

    type Input<'T when 'T :> Data<'T>> =
        abstract member Insert: 'T -> unit

    type Output<'T when 'T :> Data<'T>> =
        abstract member Get: Array<'T> -> bool

    type Source<'T when 'T :> Data<'T>> =
        abstract member Item: Ticker -> Input<'T>
        abstract member BufferLength: int
        abstract member Tickers: Ticker list

    type Store<'T when 'T :> Data<'T>> =
        abstract member Item: Ticker -> Output<'T>
        abstract member Reset: Ticker -> unit


module Wrapper =

    type private RingBuffer<'T when 'T :> Data<'T>>(ticker: Ticker,
                                                     length: int,
                                                     buffer: Data.Buffer<'T>,
                                                     verbose: bool) =
        let mutable pos: int = 0
        let mutable count: int = 0

        let data = Array.Buffer(length, fun _ -> 'T.Init())
        let get i = let k = (length + pos - i - 1) % length in data[k]

        let insert(x: 'T): unit = data[pos] <- x ; count <- count + 1
                                  pos <- pos + 1 ; if pos = length then pos <- 0

        let queue = buffer.Queue(insert)

        member internal this.Reset() =
            if verbose then Log.Info("Data", $"Reset for {ticker}")
            count <- 0 ; pos <- 0

        member internal this.Insert(x: 'T) =
            if (not <| queue.Ingest(x)) then this.Reset()

        member this.Get(l: Data.Array<'T>): bool =
                    if count < length then false else (l.Update(get) ; true)

        interface Data.Input<'T> with member this.Insert(x: 'T) = insert(x)
        interface Data.Output<'T> with member this.Get(l) = this.Get(l)


    type DataStore<'T when 'T :> Data<'T>>(tickers: Ticker list,
                                       length: int,
                                       buffer: Data.Buffer<'T>,
                                       verbose: bool) =
        let dataMap: Generic.IReadOnlyDictionary<Ticker, RingBuffer<'T>> =
            let data = Concurrent.ConcurrentDictionary<Ticker, RingBuffer<'T>>()
            for t in tickers do
                let b = data.TryAdd(t, RingBuffer(t, length, buffer, verbose))
                assert b
            data

        interface Data.Source<'T> with
            member this.Tickers: Ticker list = tickers
            member this.BufferLength: int = length
            member this.Item(ticker: Ticker): Data.Input<'T> = dataMap[ticker]

        interface Data.Store<'T> with
            member this.Item(ticker: Ticker): Data.Output<'T> = dataMap[ticker]
            member this.Reset(ticker: Ticker) = dataMap[ticker].Reset()
