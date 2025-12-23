namespace Vyapari.Tests

open System.Collections.Generic
open Vyapari


module Utils =

    let Test<'T> (f: 'T -> bool) (v: 'T list): bool =
        let check b (x: 'T): bool = if (f x) then b else false
        List.fold check true v

    let Bar (random: System.Random) (t: time) =
        let get (f: float) = Utils.Normalize(f + random.NextDouble())
        { Ask = get 3.0 ; Bid = get 1.0 ; Time = t ; Volume = 0 }

    let Gen (length: int) (min: int, max: int): IReadOnlyList<int> =
        let random = System.Random(System.Guid.NewGuid().GetHashCode())
        [| for _ in 1 .. length -> random.Next(min, max) |]
