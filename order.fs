module Tradier

open System
open System.Net.Http
open System.Text.Json

// Types
type OrderClass =
    | Equity
    | Option
    | Multileg
    | Combo

type OrderSide =
    | Buy
    | BuyToOpen
    | BuyToClose
    | Sell
    | SellShort
    | SellToOpen
    | SellToClose

type OrderType =
    | Market
    | Limit
    | Stop
    | StopLimit

type Duration =
    | Day
    | GTC
    | PreMarket
    | PostMarket

type Order =
    { Symbol: string
      Side: OrderSide
      Quantity: int
      Type: OrderType
      Price: decimal option
      Stop: decimal option }

type OTOOrder =
    { PrimaryOrder: Order
      SecondaryOrder: Order }

type OCOOrder =
    { FirstOrder: Order
      SecondOrder: Order }

type OTOCOOrder =
    { PrimaryOrder: Order
      TakeProfitOrder: Order
      StopLossOrder: Order }

module Tradier

open System
open System.Net.Http
open System.Text.Json

// Helper functions
let orderSideToString =
    function
    | Buy -> "buy"
    | BuyToOpen -> "buy_to_open"
    | BuyToClose -> "buy_to_close"
    | Sell -> "sell"
    | SellShort -> "sell_short"
    | SellToOpen -> "sell_to_open"
    | SellToClose -> "sell_to_close"

let orderTypeToString =
    function
    | Market -> "market"
    | Limit -> "limit"
    | Stop -> "stop"
    | StopLimit -> "stop_limit"

let durationToString =
    function
    | Day -> "day"
    | GTC -> "gtc"
    | PreMarket -> "pre"
    | PostMarket -> "post"

