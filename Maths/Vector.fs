namespace Vyapari.Maths

open System
open Vyapari


type Vector<'T when 'T :> IEquatable<'T>
                and 'T :> Numerics.IAdditionOperators<'T, 'T, 'T>
                and 'T :> Numerics.ISubtractionOperators<'T, 'T, 'T>
                and 'T :> Numerics.IMultiplyOperators<'T, 'T, 'T>>
                    (length: int, gen: int -> 'T) =
    let data  = Vyapari.Array.Buffer(length, gen)
    member this.Get k = data[k]
    member this.Length = length
    member this.Item(k: int) = data[k]

    static member inline (+) (v1: Vector<'V>, v2: Array<'V>): Vector<'V> =
        assert (v1.Length = v2.Length)
        let gen i = v1[i] + v2[i] in Vector(v1.Length, gen)

    static member inline (-) (v1: Vector<'V>, v2: Array<'V>): Vector<'V> =
        assert (v1.Length = v2.Length)
        let gen i = v1[i] - v2[i] in Vector(v1.Length, gen)

    static member inline (*) (v1: Vector<'V>, v2: Array<'V>): 'V =
        assert (v1.Length = v2.Length)
        let f (sum: 'V) (k: int): 'V =  sum + v1[k] * v2[k]
        let init = v1[0] * v2[0] in (List.fold f init [1 .. v1.Length - 1])

    static member inline Convolve<'V when 'V: (static member (*) : 'V * 'V -> 'V)
                                      and 'V: (static member (+) : 'V * 'V -> 'V)>
            (l: int) (f1: int -> 'V) (f2: int -> 'V): 'V =
        let sum (s: 'V) (k: int) = s + (f1 k) * (f2 k)
        let init = (f1 0) * (f2 0) in (List.fold sum init [1 .. l - 1])

    interface Array<'T> with
        member this.Item(index: int) = data[index]
        member this.Length = length
        member this.Get(index) = data[index]

    interface IEquatable<Vector<'T>> with
        override this.Equals(x: Vector<'T>): bool =
            let eq i = this[i].Equals(x[i]) in (Vyapari.Loop.Verify eq 0  length)
