namespace Vyapari

open System
open System.Net.WebSockets
open System.Text;
open System.Threading
open System.Threading.Tasks


module WebSocket =

    let maxAttempt = 3
    let receiverTimeOut = 1000
    let timeOut = 5000
    let tag = "Web Socket";


    type Config =
        abstract member Url: string with get
        abstract member Initialize: (string -> unit) -> unit
        abstract member Receiver: string * (string -> unit) -> unit
        abstract member Close: (string -> unit) -> unit


    type private Client(config: Config, cancel: unit -> unit, id: int) =

        let url: Uri = Uri(config.Url);
        let cts = new CancellationTokenSource()
        let mutable disposed = false;
        let tag = $"Web Socket [{id}]"
        let bufferLength = 4096

        let ws: ClientWebSocket =
            let webSocket= new ClientWebSocket()
            webSocket.ConnectAsync(url, cts.Token).Wait()
            Log.Info(tag, "WebSocket connected successfully")
            webSocket

        let send (message: string) =
            if (ws.State = WebSocketState.Open) then
                let msg = ArraySegment<byte>(Encoding.UTF8.GetBytes(message))
                ws.SendAsync(msg, WebSocketMessageType.Text, true, cts.Token).Wait()
            else
                Log.Warning(tag,
                    $"Unable to send message \"{message}\"; WebSocket not connected")

        let receiveLoop (ct: CancellationToken) =
            task {
                let buffer: byte[] = Array.zeroCreate bufferLength

                while (not ct.IsCancellationRequested) do
                    if (ws.State = WebSocketState.Open) then
                        try
                            let segment = ArraySegment<byte>(buffer)
                            let! result = ws.ReceiveAsync(segment, ct)

                            match result.MessageType with
                            | WebSocketMessageType.Close ->
                                let cl = WebSocketCloseStatus.NormalClosure
                                do! ws.CloseAsync(cl, "Closing", ct)
                                Log.Warning(tag, "Connection closed in receive loop")
                                cts.CancelAsync().Wait()
                            |  WebSocketMessageType.Binary ->
                                Log.Warning(tag, "Ignoring Binary data")
                            | WebSocketMessageType.Text ->
                                config.Receiver(
                                    Encoding.UTF8.GetString(buffer, 0, result.Count),
                                send)
                            | _ ->
                                let s = result.MessageType.ToString()
                                Log.Warning(tag,
                                            $"Ignoring Unknown type of message: {s}")

                        with (ex: exn) ->
                            if (not ct.IsCancellationRequested) then
                                let trace: string = ex.StackTrace.ToString()
                                Log.Warning(tag, $"Error in receive loop: {trace}")
                    else
                        Log.Warning(tag, "Socket closed in receive loop")

                if (not disposed) then
                    Async.Start <| async {
                        do! Async.Sleep(timeOut)
                        cancel()
                    }
            }

        let recv: Task = receiveLoop cts.Token

        do Log.Info(tag, "Attempting to initialize")
           config.Initialize(send)
           Log.Info(tag, "Initialization is complete")

        member this.Send msg = send msg

        interface IDisposable with
            member this.Dispose() =
                if disposed then
                    Log.Warning(tag, "Already disposed!")
                else
                    Log.Info(tag, "Initiating shutdown of connection")
                    cts.Cancel()
                    Thread.Sleep(receiverTimeOut)
                    if (ws.State = WebSocketState.Open) then
                        config.Close(send)
                        let cl = WebSocketCloseStatus.NormalClosure
                        let token = CancellationToken.None
                        ws.CloseAsync(cl, "Client closing", token).Wait()
                    ws.Dispose()
                    cts.Dispose()
                    disposed <- true
                    if (recv.IsCanceled || recv.IsFaulted || recv.IsCompleted) then
                        Log.Info(tag, "Receiver thread closed!")
                    else
                        Log.Warning(tag, "Receiver thread not closed!")


    type Connection(config: Config) =
        let mutable count = 0
        let mutable client: Client | null = null

        let rec cancel () =
            Log.Warning(tag, $"Socket connection [{count}] is shutting down!")

            if (count >= maxAttempt) then
                Log.Error(tag, "Max attempts: {MaxAttempt} reached, Shutting Down!")

            count <- count + 1
            assert (client <> null)
            let resource: IDisposable = client in resource.Dispose()
            Log.Info(tag, $"Reconnecting Socket, Attempt: {count} / {maxAttempt}")

            try (client <- new Client(config, cancel, count)) with (ex: exn) ->
                Log.Warning(tag, $"Socket [{count}] initialization failed: {ex}")
                cancel()

        do client <- new Client(config, cancel, count)

        member this.Send msg = client.Send msg

        interface IDisposable with
            member this.Dispose() =
                let resource: IDisposable = client in resource.Dispose()
