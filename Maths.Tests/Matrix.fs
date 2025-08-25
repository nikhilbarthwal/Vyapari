namespace Maths.Tests

open NUnit.Framework
open Vyapari
open Vyapari.Maths


module Matrix =

    let private count = 5

    let private identity(m: Matrix<Fraction>): bool =
        let n = m.Rows
        if n = m.Columns then
            let element i j =
                let z: Fraction = m[i][j]
                if i = j then z.IsOne() else z.IsZero()
            let check row = Loop.Verify (element row) 0 n in
            Loop.Verify check 0 n
        else false

    let private fractions (rows: int) (columns: int) (max: int): Matrix<Fraction> =
        let gen (_: int) = Array.Random columns (1, max)
        let n, d = Array.Buffer(rows, gen) , Array.Buffer(rows, gen)
        Matrix(rows, columns, fun i j -> Fraction(n[i][j], d[i][j]))

    let private shuffleTest (tag: string) (count: int) (length: int): bool =

        let suffix = $"[ Count = {count} / Length = {length} ]"
        let case k =
            let gen i j = if i = ((j + k) % length) then Fraction(1) else Fraction(0)
            let m = Matrix(length, length, gen)
            match Matrix.Invert(m) with
            | No -> Log.Warning(tag, $"Shuffle failed inverse for {count}") ; false
            | Yes(x) -> identity(x * m)
        if Loop.Verify case 0 length then
            Log.Info(tag, $"Passed {suffix}") ; true
        else
            Log.Info(tag, $"Failed {suffix}") ; false

    let private inverseTest (tag: string) (count: int) (length: int): bool =
        let suffix = $"[ Count = {count} / Length = {length} ]"
        let m = fractions length length 500
        match Matrix.Invert(m) with
        | No -> Log.Info(tag, $"Skipping inverse test {suffix}") ; true
        | Yes(x) -> if identity(x * m) then
                         Log.Info(tag, $"Passed {suffix}") ; true
                    else
                        Log.Warning(tag, $"Failed {suffix}") ; false

    let private vanderTest (tag: string) (count: int) (length: int) order: bool =
        let suffix = $"[ Count = {count} / Length = {length} / Order = {order} ]"
        let input: Vector<Fraction> = let m = fractions 1 length 500 in m[0]
        let m = Matrix.Vandermonde<Fraction>(order, input)
        let gen power pos = input[pos].Pow(power) :> System.IComparable<Fraction>
        let element i j = (gen i j).CompareTo(m[i][j]) = 0
        let check row = Loop.Verify (element row) 0 length
        if Loop.Verify check 0 (order + 1) then
            Log.Info(tag, $"Passed {suffix}") ; true
        else
            Log.Info(tag, $"Failed {suffix}") ; false


    [<Test>]
    let Shuffle() = let tag = "Matrix Shuffle Test"
                    let tests = Array.Random count (10, 50)
                    let check k = shuffleTest tag k tests[k]
                    Assert.True(Loop.Verify check 0 count)

    [<Test>]
    let Inverse() = let tag = "Matrix Inverse Test"
                    let tests = Array.Random count (10, 20)
                    let check k = inverseTest tag k <| tests[k]
                    Assert.True(Loop.Verify check 0 count)

    [<Test>]
    let rec Vandermonde() = let tag = "Matrix Vandermonde Test"
                            let length = Array.Random count (10, 200)
                            let power = Array.Random count (2, 10)
                            let check k = vanderTest tag k length[k] power[k]
                            Assert.True(Loop.Verify check 0 count)
