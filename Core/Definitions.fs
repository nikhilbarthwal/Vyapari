namespace Vyapari.Core

open System
open Vyapari


[<Struct>] type OptionType = Call | Put
    with override this.ToString() = match this with Call -> "Call" | Put -> "Put"

type Ticker =
    | Stock of Symbol: string
    | Option of Symbol: string * Strike: float * Expiry: DateTime * Type: OptionType
    | Crypto of Symbol: string
    with
        override this.ToString() =
            match this with
            | Stock(symbol) ->
                $"Type: Stock / Symbol: {symbol}"
            | Option(symbol, strike, expiry, direction) ->
                let exp = expiry.ToString("yyyy-MM-dd")
                $"Type: Option / Direction: {direction.ToString()} / Symbol: " +
                $"{symbol} / Strike: {strike} / Expiry: {exp}"
            | Crypto(symbol) ->
                $"Type: Crypto / Symbol: {symbol}"

        member this.Symbol =
            match this with
            | Stock(symbol) -> symbol
            | Option(symbol, _, _, _) -> symbol
            | Crypto(symbol) -> symbol


module Order =

    [<Struct>] // TODO: Should not be Struct but reference type
    type Entry (ticker: Ticker, quantity: uint, price: float,
                profit: float, loss: float) =

        member this.Ticker = ticker
        member this.Quantity = quantity
        member this.Price = assert (price > 0) ; Utils.Normalize(price)
        member this.Profit = Utils.Normalize(profit)
        member this.Loss = Utils.Normalize(loss)
        member this.ProfitPercent() = (100.0 * (this.Profit - this.Price))/this.Price
        member this.LossPercent() = (100.0 * (this.Price - this.Loss))/this.Price
        with
            override this.ToString() =
                $"Order = {this.Ticker} -> Quantity: {this.Quantity} / Price: " +
                $"{this.Price} / ProfitPrice: {this.Profit} / LossPrice: {this.Loss}"
            static member Compare (a: Maybe<Entry>) (b: Maybe<Entry>) =
                match a, b with
                | No, No -> No
                | Yes(o), No -> Yes(o)
                | No, Yes(o) -> Yes(o)
                | Yes(o1), Yes(o2) ->
                    Yes(if o1.ProfitPercent() > o2.ProfitPercent() then o1 else o2)

(*
[<Struct>] type AccountInfo = { Total: float ; Profit: float }


module Order =

    [<Struct>]
    type Entry (param: struct {| Ticker: Ticker; Quantity: uint; Price: float
                                 Profit: float; Loss: float |}) =

        member this.Ticker = param.Ticker
        member this.Quantity: int = int param.Quantity
        member this.Price = assert (param.Price > 0) ; Utils.Normalize(param.Price)
        member this.Profit = Utils.Normalize(param.Profit)
        member this.Loss = Utils.Normalize(param.Loss)
        member this.ProfitPercent() = (100.0 * (this.Profit - this.Price))/this.Price
        member this.LossPercent() = (100.0 * (this.Price - this.Loss))/this.Price
        with override this.ToString() =
                $"Order = {this.Ticker} -> Quantity: {this.Quantity} / Price: " +
                $"{this.Price} / ProfitPrice: {this.Profit} / LossPrice: {this.Loss}"

    [<Struct>] type Status = Placed | Triggered | Executed | Cancelled


type Client<'T> =
    abstract AccountInfo: unit -> AccountInfo
    abstract CancelOrder: 'T -> bool
    abstract OrderStatus: 'T -> Order.Status
    abstract PlaceOrder: Order.Entry -> 'T
*)
