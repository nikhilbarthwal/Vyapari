namespace Vyapari

open System
open System.Diagnostics
open System.Collections.Generic

type time = int64

[<Struct>] type Maybe<'T> = Yes of 'T | No

module Utils =

    let inline CreateDictionary<'V, 'K when 'K: equality>(keys: IEnumerable<'K>,
                                                          gen: 'K -> 'V) =
        let data = Dictionary<'K, 'V>()
        for key in keys do data.Add(key, gen key)
        data :> IReadOnlyDictionary<'K,'V>

    let inline Normalize(x: float): Decimal = decimal <| Math.Round(x, 3)

    let inline Ascii (inp: string): string =
        let bytes = System.Text.Encoding.ASCII.GetBytes(inp)
        System.Text.Encoding.UTF8.GetString(bytes, 0, bytes.Length).Replace("?", " ")

    let inline ToDateTime(epoch: int64): DateTime =
        let dateTimeOffset  = DateTimeOffset.FromUnixTimeSeconds(epoch)
        let estZone = TimeZoneInfo.FindSystemTimeZoneById("Eastern Standard Time")
        TimeZoneInfo.ConvertTimeFromUtc(dateTimeOffset.DateTime, estZone)

    let inline Wait (timeout: int) =
        assert (timeout > 0) ; Threading.Thread.Sleep(timeout * 1000)

    let inline Diff (a: float, b: float) = Normalize(100.0 * (a - b) / b)

    let inline CurrentTime() = DateTime.Now.ToString("F")

    let inline Max (f1:float) (f2: float): float = if f1 > f2 then f1 else f2


module Log =

    type private log() =
        do Trace.Listeners.Add(new ConsoleTraceListener(true)) |> ignore

        member this.Entry (header: string) (tag: string, msg: string): unit =
            let timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff")
            let tagStr = if tag = "" then "" else $" {tag}"
            Trace.WriteLine($"[{timestamp}] {header}{tagStr}: {msg}")

    let private logger = log()

    let Warning = logger.Entry "WARNING"
    let Info = logger.Entry "INFO"

#if DEBUG
    let Debug = logger.Entry "Debug"
#endif

    let Error(tag, msg) =
        logger.Entry "ERROR" (tag, msg) ; raise <| Exception(msg)

    let Exception(tag, msg, ex: exn) =
        logger.Entry "EXCEPTION" (tag, msg) ; raise <| Exception(msg, ex)
