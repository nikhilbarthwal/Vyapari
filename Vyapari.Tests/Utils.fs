namespace Vyapari.Tests

open Vyapari


module Utils =

    let Test<'T> (f: 'T -> bool) (v: 'T list): bool =
        let check b (x: 'T): bool = if (f x) then b else false
        List.fold check true v

    let Bar (random: System.Random) (t: time): DataPoint =
        { Ask = decimal <| 3.0 + random.NextDouble()
          Bid = decimal <| 1.0 + random.NextDouble()
          Time = t
          Volume = 0}

    let Gen (length: int) (min: int, max: int): Array<int> =
        let random = System.Random(System.Guid.NewGuid().GetHashCode())
        Array.Initialize(length, fun _ -> random.Next(min, max))
