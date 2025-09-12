namespace Vyapari


open System.Diagnostics

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
