namespace Vyapari

open System
open System.Net.WebSockets
open System.Threading
open System.Text


module Socket =

    type Config =
        abstract Url: string
        abstract Initialize: (string -> unit) -> unit
        abstract Receiver: string * (string -> unit) -> unit
        abstract Tag: string
        abstract Close: (string -> unit) -> unit


    type private Client(z: Config) =

        let send (cl: ClientWebSocket) (message: string) =
            if (cl.State = WebSocketState.Open) then
                let bytes = Encoding.UTF8.GetBytes(message);
                let buffer = ArraySegment<byte>(bytes);
                cl.SendAsync(buffer, WebSocketMessageType.Text, true,
                             CancellationToken.None).Wait()
                Log.Info(z.Tag, $"Sent: {message}")
            else
                Log.Warning(z.Tag, "Connection is not open.")
                // TODO: Initiate shutdown


        let receiver (cl: ClientWebSocket) =
            let buffer: byte[] = Array.zeroCreate 8192
            try
                while cl.State = WebSocketState.Open do
                    let result = cl.ReceiveAsync(ArraySegment<byte>(buffer),
                                                     CancellationToken.None).Result
                    if (result.MessageType = WebSocketMessageType.Close) then
                        failwith "Server initiated close."
                    else
                        if (result.MessageType = WebSocketMessageType.Text) then
                            let m = Encoding.UTF8.GetString(buffer, 0, result.Count)
                            z.Receiver(m, send cl)
            with ex ->
                Log.Warning(z.Tag, $"Error: {ex.Message}")
                // TODO: Initiate shutdown

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
                Log.Exception(z.Tag,
                              $"Error in {z.Url} socket connection: {ex.Message}",
                              ex)

        member this.Send(msg: string) = send client msg

        member this.IsAlive: bool = client.State = WebSocketState.Open

        member this.Close() =
            try
                z.Close(this.Send)
                client.CloseAsync(WebSocketCloseStatus.NormalClosure,
                                  "Client closing", CancellationToken.None).Wait()
                receiver.Join()
                if this.IsAlive then
                    Log.Warning(z.Tag, $"Socket connection for {z.Url} still alive")
                else Log.Info(z.Tag, $"Closed Socket connection for {z.Url}")
            with ex ->
                Log.Warning(z.Tag, $"Failed to close connection for {z.Url}")

        interface IDisposable with member this.Dispose() = this.Close()

    type Connection(z: Config) =

        let timeout = 10
        let mutable attempts = 0
        let maxAttempts = 3

        let mutable client: Maybe<Client> = No

        let rec reconnect() =
            match client with
            | Yes(cl) -> cl.Close()
            | No -> ()

            if attempts >= maxAttempts then
                Log.Error(z.Tag,
                          $"Unable to reconnect to {z.Url} after {maxAttempts}")
            else
                Utils.Wait(timeout)
                Log.Warning(z.Tag,
                    $"Reconnecting to {z.Url} (Attempt: {attempts}/{maxAttempts})")
                let cl = new Client(z)
                if cl.IsAlive then client <- Yes(cl) else cl.Close()
                reconnect()

        do client <- Yes(new Client(z))

        member this.Send(msg: string) =
            match client with
            | Yes(cl) -> cl.Send msg
            | No -> Log.Warning(z.Tag, $"Attempting to send message = {msg} to " +
                                       $"access uninitialized connection {z.Url}")

        member this.Dispose() =
            match client with Yes(cl) -> cl.Close() ; client <- No | No -> ()

        interface IDisposable with member this.Dispose()= this.Dispose()
