namespace Vyapari.Core.Tests

open Vyapari
open Vyapari.Core


module Utils =

    let internal Test<'T> (f: 'T -> bool) (v: 'T list): bool =
        let check b (x: 'T): bool = if (f x) then b else false
        List.fold check true v

    let internal Bar (random: System.Random) (t: time) =
        DataPoint(ask = 3.0 + random.NextDouble(),
                  bid = 1.0 + random.NextDouble(),
                  time = t,
                  volume = 0)
