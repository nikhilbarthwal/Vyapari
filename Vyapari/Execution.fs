namespace Vyapari


// type Strategy = abstract Eval: float -> Maybe<Order>


type Client<'T when 'T :> Data<'T>> =
    inherit System.IDisposable
    abstract DataSource: Data.Source<'T>


(*
    abstract IsAlive: bool
    abstract AccountBalance: unit -> float
    abstract PlaceOrder: Order -> int
    abstract CancelOrder: 'T -> bool
    //abstract OrderStatus: 'T -> Order.Status



type Session<'T when 'T :> Data<'T>>(strategyGen: Data.Source<'T> -> Strategy,
                                     initialCapital: float,
                                     targetCapital: float,
                                     lossThreshold: float,
                                     startTime: Maybe<System.DateTime>,
                                     endTime: Maybe<System.DateTime>) =

    let continueExecutionLoop(balance: float): bool =
        if balance <= lossThreshold then false else
             match endTime with Yes(time) -> time > System.DateTime.Now | No -> true

    [<TailCall>]
    let rec waitUntil (time: System.DateTime): unit =
        if System.DateTime.Now < time then (Utils.Wait(60) ; waitUntil(time))

    let execute (client: Client<'T>) (strategy: Strategy): int =
        let timestamp = Utils.CurrentTime()
        Log.Info("Session", "Starting Trading session with Initial capital" +
                           $" = {initialCapital} at {timestamp}")

        while (continueExecutionLoop <| client.AccountBalance()) do
            match strategy.Eval(initialCapital) with
            | No -> Utils.Wait(1)
            | Yes(order) -> Log.Info("Order", $"Placed -> {order}") ; Utils.Wait(5)

        let time = Utils.CurrentTime()
        let balance = client.AccountBalance()
        if client.AccountBalance() >= targetCapital then
            Log.Info("Session", "Successfully ending Trading session with " +
                               $"Final balance = {balance} at {time}") ; 0
        else
            Log.Warning("Session", "Unsuccessfully ending Trading session with" +
                                  $"Final balance = {balance} at {time}") ; 1

    member this.Simulate(source: Data.Source<'T>): bool = true //TODO

    member this.Evaluate(filename: string): bool = true // TODO

    member this.Execute(client: Client<'T>): int =
        if client.IsAlive then
            let balance = client.AccountBalance()
            let timestamp = Utils.CurrentTime()
            if balance < initialCapital then
                Log.Warning("Session", "Unable to start session as Initial capital" +
                                      $" = {initialCapital} is below Account " +
                                      $"Balance = {balance} at {timestamp}") ; 2
            else
                Log.Info("Session", "Initializing strategy")
                let strategy: Strategy = strategyGen <| client.DataSource
                match startTime with
                | No -> execute client strategy
                | Yes(time) ->
                    let timestamp = time.ToString("F")
                    Log.Info("Session", $"Waiting till {timestamp} to start")
                    waitUntil(time)
                    execute client strategy
        else Log.Warning("Session", "Client is not alive!") ; 3


abstract class Strategy<T>(source: DataSource) =
    abstract public Eval: float -> Order?

interface Execution.Params: IDisponsible =
    abstract public Start: (capital: float, DateTime) -> bool
    abstract public Stop: (capital: float, DateTime) -> bool
    abstract public Target: (capital: float, DateTime) -> bool
    abstract public DataSource: unit -> Wrapper
    abstract public Strategy: Wrapper -> Strategy
    abstract public Client: unit -> Client

    static Run: Execution -> int

// CoinBase/Tradier/Alpaca will extend Execution.Param for its one purpose
Main bot:
let Main _ = use (execute = {
    new Execution.Params with
        bool Start() = custom code ...
}) Execution.Run

*)
