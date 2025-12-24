namespace Vyapari

open System.Collections


type Data<'T when 'T :> Data<'T>> = abstract member Price: decimal
                                    abstract member Time: time

module Data =

    type Array<'T when 'T :> Data<'T>>(length: int, f: unit -> 'T) =
        let data = [| for _ in 1 .. length -> f() |]
        member inline internal this.Overwrite(get: int -> 'T) =
            for i in 0 .. length - 1 do data[i] <- get i

        interface Generic.IReadOnlyList<'T> with
            member this.Count = data.Length
            member this.Item with get index = data[index]

            member this.GetEnumerator() = (data :> seq<'T>).GetEnumerator()
            member this.GetEnumerator() = (data :> IEnumerable).GetEnumerator()


    module Buffer =
        [<Struct>] type Verbosity = Raw | Final | All
        type Queue<'T when 'T :> Data<'T>> = abstract member Ingest: 'T -> bool

    type Buffer<'T when 'T :> Data<'T>> =
        abstract member CreateQueue: Ticker * ('T -> unit) -> Buffer.Queue<'T>
        abstract member Init: unit -> 'T

    type Input<'T when 'T :> Data<'T>> =
        abstract member Insert: 'T -> unit

    type Output<'T when 'T :> Data<'T>> =
        abstract member Get: Array<'T> -> bool

    type Source<'T when 'T :> Data<'T>> =
        abstract member Item: Ticker -> Output<'T>
        abstract member BufferLength: int
        abstract member Tickers: Ticker list

    type Store<'T when 'T :> Data<'T>> =
        abstract member Item: Ticker -> Input<'T>
        abstract member Reset: Ticker -> unit
        abstract member Tickers: Ticker list


    type private RingBuffer<'T when 'T :> Data<'T>>
            (ticker: Ticker, length, buffer: Buffer<'T>) =
        let mutable pos: int = 0
        let mutable count: int = 0
        let object = System.Object()

        let data = [| for _ in 1 .. length -> buffer.Init() |]
        let get i = let k = (length + pos - i - 1) % length in data[k]

        let queue: Buffer.Queue<'T> =
            let insert(x: 'T) () = data[pos] <- x ; count <- count + 1
                                   pos <- pos + 1 ; if pos = length then pos <- 0
            let ingest x = lock object (insert x)
            buffer.CreateQueue(ticker, ingest)

        member inline internal this.Reset() =
            let reset() = (count <- 0 ; pos <- 0) in (lock object reset)

        member inline internal this.Insert(x: 'T) =
            if (not <| queue.Ingest(x)) then this.Reset()

        member inline this.Get(l: Array<'T>): bool =
            if count < length then false else
                lock object (fun _ -> l.Overwrite(get)) ; true

        interface Input<'T> with member this.Insert(x: 'T) = this.Insert(x)
        interface Output<'T> with member this.Get(l) = this.Get(l)


    type Map<'T when 'T :> Data<'T>>
            (tickers: Ticker list, length: int, buffer: Buffer<'T>) =

        let dataMap: Generic.IReadOnlyDictionary<Ticker, RingBuffer<'T>> =
            let data = Concurrent.ConcurrentDictionary<Ticker, RingBuffer<'T>>()
            for ticker in tickers do
                let b = data.TryAdd(ticker, RingBuffer(ticker, length, buffer))
                assert b
            data

        interface Source<'T> with
            member this.Tickers: Ticker list = tickers
            member this.BufferLength: int = length
            member this.Item(ticker: Ticker): Output<'T> = dataMap[ticker]

        interface Store<'T> with
            member this.Tickers: Ticker list = tickers
            member this.Item(ticker: Ticker): Input<'T> = dataMap[ticker]
            member this.Reset(ticker: Ticker) = dataMap[ticker].Reset()
