namespace Vyapari

open System


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
                let expiryStr = expiry.ToString("yyyy-MM-dd")
                $"Type: Option / Direction: {direction.ToString()} / Symbol: " +
                $"{symbol} / Strike: {strike} / Expiry: {expiryStr}"
            | Crypto(symbol) ->
                $"Type: Crypto / Symbol: {symbol}"

        member this.Symbol =
            match this with
            | Stock(symbol) -> symbol
            | Option(symbol, _, _, _) -> symbol
            | Crypto(symbol) -> symbol
