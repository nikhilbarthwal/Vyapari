namespace Vyapari

open System
open System.Net.WebSockets
open System.Threading
open System.Text


module Socket =

    type Adapter =
        abstract Url: string
        abstract Initialize: (string -> unit) -> unit
        abstract Receiver: string * (string -> unit) -> unit
        abstract Tag: string
        abstract Close: (string -> unit) -> unit

    type Connection(z: Adapter) =

        let send (cl: ClientWebSocket) (message: string) =
            if (cl.State = WebSocketState.Open) then
                let bytes = Encoding.UTF8.GetBytes(message);
                let buffer = ArraySegment<byte>(bytes);
                cl.SendAsync(buffer, WebSocketMessageType.Text, true,
                             CancellationToken.None).Wait()
                Log.Info(z.Tag, $"Sent: {message}")
            else Log.Info(z.Tag, "Connection is not open.")

        let receiver (cl: ClientWebSocket) =
            let buffer: byte[] = Array.zeroCreate 8192
            let mutable b = true
            try
                while b && (cl.State = WebSocketState.Open) do
                    let result = cl.ReceiveAsync(ArraySegment<byte>(buffer),
                                                     CancellationToken.None).Result
                    if (result.MessageType = WebSocketMessageType.Close) then
                        Log.Info(z.Tag, "\nServer initiated close.")
                        b <- false
                    else
                        if (result.MessageType = WebSocketMessageType.Text) then
                            let msg = Encoding.UTF8.GetString(buffer,0, result.Count)
                            z.Receiver(msg, send cl)
                with ex -> Log.Info(z.Tag, $"Error: {ex.Message}")

        let client, receiver =
            try
                use cl = new ClientWebSocket()
                Log.Info(z.Tag, $"Connecting to {z.Url}...")
                cl.ConnectAsync(Uri(z.Url), CancellationToken.None).Wait()
                Log.Info(z.Tag, "Connected!")
                let rc = Thread(fun () -> receiver cl)
                rc.Start()
                z.Initialize(send cl)
                (cl, rc)
            with ex ->
                Log.Exception(z.Tag, $"Exception in {z.Url} socket connection", ex)

        member this.Send(msg: string) = send client msg

        member this.Close() =
            try
                z.Close(this.Send)
                client.CloseAsync(WebSocketCloseStatus.NormalClosure,
                                  "Client closing", CancellationToken.None).Wait()
                receiver.Join()
                Log.Info(z.Tag, $"Closed Socket connection for {z.Url}")
            with ex ->
                Log.Warning(z.Tag, $"Failed to close connection for {z.Url}")

        interface IDisposable with member this.Dispose() = this.Close()
