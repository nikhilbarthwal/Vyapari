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

    type private Connect(z: Adapter, reconnect: int -> unit, timeout: int) =

        let send (cl: ClientWebSocket) (message: string) =
            if (cl.State = WebSocketState.Open) then
                let bytes = Encoding.UTF8.GetBytes(message);
                let buffer = ArraySegment<byte>(bytes);
                cl.SendAsync(buffer, WebSocketMessageType.Text, true,
                             CancellationToken.None).Wait()
                Log.Info(z.Tag, $"Sent: {message}")
            else
                Log.Info(z.Tag, "Connection is not open.")
                reconnect 1

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
                with ex ->
                    Log.Info(z.Tag, $"Error: {ex.Message}")
                    reconnect 1

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

        member this.IsAlive: bool = client.State = WebSocketState.Open

        member this.Close() =
            try
                z.Close(this.Send)
                client.CloseAsync(WebSocketCloseStatus.NormalClosure,
                                  "Client closing", CancellationToken.None).Wait()
                Utils.Wait timeout
                receiver.Join()
                if this.IsAlive then
                    Log.Warning(z.Tag, $"Socket connection for {z.Url} still alive")
                else Log.Info(z.Tag, $"Closed Socket connection for {z.Url}")
            with ex ->
                Log.Warning(z.Tag, $"Failed to close connection for {z.Url}")

        interface IDisposable with member this.Dispose() = this.Close()

    type Connection(z: Adapter) =

        let timeout = 15
        let maxReconnectAttempts = 3

        let mutable connection: Maybe<Connect> = No

        let rec reconnect attempt: unit =
            match connection with
            | Yes(conn) -> conn.Close()
            | No -> ()

            if attempt > maxReconnectAttempts then
                Log.Exception(z.Tag, $"Unable to reconnect to {z.Url}",
                              Exception("$Unable to reconnect to {z.Url}"))
            else
                Log.Warning(z.Tag, $"Reconnecting to {z.Url}, Attempt {attempt}")
                let conn = new Connect(z, reconnect, timeout)
                if conn.IsAlive then connection <- Yes(conn) else
                    conn.Close()
                    reconnect <| attempt + 1

        do connection <- Yes(new Connect(z, reconnect, timeout))

        member this.Send(msg: string) =
            match connection with
            | Yes(conn) -> conn.Send msg
            | No ->
                let err = $"Attempting to access uninitialized connection {z.Url}"
                Log.Exception(z.Tag, err, Exception(err))

        member this.Dispose() =
            match connection with Yes(conn) -> conn.Close() ; connection <- No
                                | No -> ()

        interface IDisposable with member this.Dispose()= this.Dispose()
