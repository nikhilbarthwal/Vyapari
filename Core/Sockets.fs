namespace Vyapari.Core

open Vyapari
open Websocket.Client


module Socket =

    type Adapter =
        abstract Timeout: int
        abstract Url: string
        abstract Initialize: (string -> bool) -> unit
        abstract Receive: string * (string -> bool) -> unit
        abstract Reconnect: string * (string -> bool) -> unit
        abstract Tag: string
        abstract Close: (string -> bool) -> unit


    type Connection(z: Adapter) =

        let client, reconnect, receive, task =
            try
                let cl = new WebsocketClient(System.Uri(z.Url))
                cl.ReconnectTimeout <- System.TimeSpan.FromSeconds(z.Timeout)

                let rc = System.ObservableExtensions.Subscribe(
                             cl.ReconnectionHappened,
                             fun info -> z.Reconnect(info.Type.ToString(), cl.Send))

                let rv = System.ObservableExtensions.Subscribe(
                             cl.MessageReceived,
                             fun msg -> z.Receive(msg.Text, cl.Send))

                let tsk = cl.Start()
                z.Initialize(cl.Send)
                (cl, rc, rv, tsk)

            with ex ->
                Log.Exception(z.Tag, $"Exception in {z.Url} socket connection", ex)

        member this.Send(msg: string) = client.Send msg

        interface System.IDisposable with
            member this.Dispose() =
                z.Close(client.Send)
                receive.Dispose() ; reconnect.Dispose() ; client.Dispose()
                Utils.Wait(z.Timeout)
                if task.IsCompleted then
                    Log.Info(z.Tag, $"Closed Socket connection for {z.Url}")
                else
                    Log.Warning(z.Tag, $"Failed to close connection for {z.Url}")
