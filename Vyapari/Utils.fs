namespace Vyapari

open System
open System.Diagnostics


type time = int64

[<Struct>] type Maybe<'T> = Yes of 'T | No

module Utils =

    let inline CreateDictionary<'V, 'K when 'K: equality>(l: 'K list, f: 'K -> 'V) =
        let data = Collections.Concurrent.ConcurrentDictionary<'K, 'V>(l.Length)
        for x in l do data.Add(x, f x)
        data :> System.Collections.Generic.IReadOnlyDictionary<'K,'V>

    let inline Normalize(x: float) = Math.Round(x, 3)

    let BisectFloat (r1: int) (r2: int) (v1: float) (v2: float): float =
        (v1 * (float r1) + v2 * (float r2)) / (float <| r1 + r2)

    let BisectLong (r1: int) (r2: int) (v1: int64) (v2: int64): int64 =
        (v1 * (int64 r1) + v2 * (int64 r2)) / (int64 <| r1 + r2)

    let Ascii (inp: string): string =
        let bytes = System.Text.Encoding.ASCII.GetBytes(inp)
        System.Text.Encoding.UTF8.GetString(bytes, 0, bytes.Length).Replace("?", " ")

    let ToDateTime(epoch: int64): DateTime =
        let dateTimeOffset  = DateTimeOffset.FromUnixTimeSeconds(epoch)
        let estZone = TimeZoneInfo.FindSystemTimeZoneById("Eastern Standard Time")
        TimeZoneInfo.ConvertTimeFromUtc(dateTimeOffset.DateTime, estZone)

    let inline Wait (timeout: int) =
        assert (timeout > 0) ; Threading.Thread.Sleep(timeout * 1000)

    let inline Diff (a: float, b: float) = Normalize(100.0 * (a - b) / b)

    let inline CurrentTime() = DateTime.Now.ToString("F")

    let inline Max (f1:float) (f2: float): float = if f1 > f2 then f1 else f2


module Loop =

    [<TailCall>]
    let rec Verify (f: int -> bool) (a: int) (b: int): bool =
        if a = b then true else
            if (f a) then (Verify f (a + 1) b) else false

    [<TailCall>]
    let rec Search (f: int -> bool) (a: int) (b: int): int =
        if a = b then b else
            if (f a) then a else (Search f (a + 1) b)

type Array<'T> = abstract member Item: int -> 'T
                 abstract member Length: int
                 abstract member Get: int -> 'T


module Array =

    type Buffer<'T>(length: int, gen: int -> 'T) =
        let data = [| for i in 0 .. length - 1 -> gen i |]

        member this.Overwrite(f: int -> 'T) =
            for i in 0 .. length - 1 do data[i] <- f i

        member this.Length = length

        member this.Item
            with get(index: int) =  data[index]
            and set(index: int) (value: 'T) = data[index] <- value

        member this.Get(index) = data[index]

        interface Array<'T> with
            member this.Item(index: int) = data[index]
            member this.Length = length
            member this.Get(index) = data[index]

    let Initialize<'T>(length: int, gen: int -> 'T): Array<'T> = Buffer(length, gen)


module Log =

    type private log() =
        do Trace.Listeners.Add(new ConsoleTraceListener(true)) |> ignore

        member this.Entry (header: string) (tag: string, msg: string): unit =
            let timestamp = System.DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff")
            let tagStr = if tag = "" then "" else $" {tag}"
            Trace.WriteLine($"[{timestamp}] {header}{tagStr}: {msg}")

    let private logger = log()
    let Warning = logger.Entry "WARNING"
    let Info = logger.Entry "INFO"

#if DEBUG
    let Debug = logger.Entry "Debug"
#endif

    let Error(tag, msg) =
        logger.Entry "EXCEPTION" (tag, msg) ; raise (System.Exception(msg))

    let Exception(tag, msg, ex: exn) =
        logger.Entry "EXCEPTION" (tag, msg) ; raise (System.Exception(msg, ex))
