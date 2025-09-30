namespace Vyapari.Tests

open Vyapari


module Utils =

    let Test<'T> (f: 'T -> bool) (v: 'T list): bool =
        let check b (x: 'T): bool = if (f x) then b else false
        List.fold check true v

    let Bar (random: System.Random) (t: time) =
        DataPoint(ask = 3.0 + random.NextDouble(),
                  bid = 1.0 + random.NextDouble(),
                  time = t,
                  volume = 0)

    let Gen (length: int) (min: int, max: int): Array<int> =
        let random = System.Random(System.Guid.NewGuid().GetHashCode())
        Array.Initialize(length, fun _ -> random.Next(min, max))
