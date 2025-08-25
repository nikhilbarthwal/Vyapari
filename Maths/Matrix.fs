namespace Vyapari.Maths

open System
open Vyapari


type Matrix<'T when 'T :> IEquatable<'T>
                and 'T :> Numerics.IAdditionOperators<'T, 'T, 'T>
                and 'T :> Numerics.ISubtractionOperators<'T, 'T, 'T>
                and 'T :> Numerics.IMultiplyOperators<'T, 'T, 'T>>
        (rows: int, columns: int, gen: int -> int -> 'T) =

    let data =
        let row i = Vector(columns, gen i) in Vyapari.Array.Buffer(rows, row)

    member this.Item with get k = data[k]
    member this.Row i j = data[i][j]
    member this.Column j i = data[i][j]
    member this.Rows = rows
    member this.Columns = columns

    member this.Multiply(output: Array.Buffer<'T>, input: Array<'T>): unit =
        let f i = this[i] * input in output.Overwrite(f)

    member this.Transform(f: 'T -> 'V): Matrix<'V> =
        Matrix(rows, columns, fun i j -> (f <| data[i][j]))

    static member inline (+) (m1: Matrix<'V>, m2: Matrix<'V>): Matrix<'V> =
        assert ((m1.Rows = m2.Rows) && (m1.Columns = m2.Columns))
        let gen i j = m1[i][j] + m2[i][j] in Matrix(m1.Rows, m2.Columns, gen)

    static member inline (-) (m1: Matrix<'V>, m2: Matrix<'V>): Matrix<'V> =
        assert ((m1.Rows = m2.Rows) && (m1.Columns = m2.Columns))
        let gen i j = m1[i][j] - m2[i][j] in Matrix(m1.Rows, m2.Columns, gen)

    static member inline (*) (m1: Matrix<'V>, m2: Matrix<'V>): Matrix<'V> =
        assert (m2.Rows = m1.Columns)
        let gen i j = Vector<'V>.Convolve m2.Rows (m1.Row i) (m2.Column j)
        Matrix(m1.Rows, m2.Columns, gen)

    static member Transpose(m: Matrix<'T>) = Matrix(m.Columns, m.Rows, m.Column)

    interface Numerics.IAdditionOperators<Matrix<'T>, Matrix<'T>, Matrix<'T>> with
        static member (+) (m1, m2) = m1 + m2

    interface Numerics.ISubtractionOperators<Matrix<'T>, Matrix<'T>, Matrix<'T>> with
        static member (-) (m1, m2) = m1 - m2

    interface Numerics.IMultiplyOperators<Matrix<'T>, Matrix<'T>, Matrix<'T>> with
        static member (*) (m1, m2) = m1 * m2

    interface IEquatable<Matrix<'T>> with
        override this.Equals(x: Matrix<'T>): bool =
            if (this.Rows = x.Rows) && (this.Columns = x.Columns) then
                let eq i = this[i].Equals(x[i]) in (Loop.Verify eq 0  x.Rows)
            else false


module Matrix =

    type Array<'T> = Vyapari.Array.Buffer<'T>

    let Vandermonde<'T when 'T :> System.Numerics.IMultiplicativeIdentity<'T, 'T>
                        and 'T :> Numerics.IAdditionOperators<'T, 'T, 'T>
                        and 'T :> Numerics.ISubtractionOperators<'T, 'T, 'T>
                        and 'T :> Numerics.IMultiplyOperators<'T, 'T, 'T>
                        and 'T :> IEquatable<'T>>
            (order: int, input: Vector<'T>): Matrix<'T> =
        let ones() = Array<'T>(input.Length, fun _ -> 'T.MultiplicativeIdentity)
        let output = [| for _ in 0 .. order -> ones() |]
        let element row k = output[row - 1][k] * input[k]
        for row in 1 .. order do output[row].Overwrite(element row)
        Matrix(order + 1, input.Length, fun i j -> output[i][j])


    type private GaussElimination<
       'T when 'T :> IEquatable<'T>
           and 'T :> Numerics.IAdditiveIdentity<'T, 'T>
           and 'T :> Numerics.IMultiplicativeIdentity<'T, 'T>
           and 'T :> Numerics.IAdditionOperators<'T, 'T, 'T>
           and 'T :> Numerics.ISubtractionOperators<'T, 'T, 'T>
           and 'T :> Numerics.IMultiplyOperators<'T, 'T, 'T>
           and 'T :> Numerics.IDivisionOperators<'T, 'T, 'T>
       > (input: Array<Array<'T>>, output: Array<Array<'T>>) =

        let isZero (i, j) =
            let z = input[i][j] in z.Equals('T.AdditiveIdentity)

        member this.Size = input.Length - 1
        member this.Output(k) = Matrix(k, k, fun i j -> output[i][j] / input[i][i])

        member this.Eliminate(i, j) =
            if (i <> j) && (not <| isZero(j, i)) then
                let factor = input[j][i] / input[i][i]
                let update (m: Array<Array<'T>>) k = m[j][k] - factor * m[i][k]
                input[j].Overwrite(update input)
                output[j].Overwrite(update output)
                assert isZero(j, i)

    let Invert<'T when 'T :> IEquatable<'T>
                   and 'T :> Numerics.IAdditiveIdentity<'T, 'T>
                   and 'T :> Numerics.IMultiplicativeIdentity<'T, 'T>
                   and 'T :> Numerics.IAdditionOperators<'T, 'T, 'T>
                   and 'T :> Numerics.ISubtractionOperators<'T, 'T, 'T>
                   and 'T :> Numerics.IMultiplyOperators<'T, 'T, 'T>
                   and 'T :> Numerics.IDivisionOperators<'T, 'T, 'T>>(m: Matrix<'T>):
                       Maybe<Matrix<'T>> =
        if  (m.Rows <> m.Columns) then No else
            let zero = 'T.AdditiveIdentity
            let one = 'T.MultiplicativeIdentity
            let identity (i: int) (j: int) = if i = j then one else zero
            let inputRow row = Array<'T>(m.Columns, fun j -> m[row][j])
            let input = Array<Array<'T>>(m.Rows, inputRow)
            let outputRow row = Array<'T>(m.Columns, identity row)
            let output = Array<Array<'T>>(m.Rows, outputRow)
            let notZero j i = let x: 'T = input[i][j] in not <| x.Equals(zero)

            let shuffle k: bool =
                if (notZero k k) then true else
                    let p = Loop.Search (notZero k) (k+1) m.Rows
                    if p = m.Rows then false else
                        input.Swap(k, p) ; output.Swap(k, p)
                        assert (notZero k k) ; true

            if Loop.Verify shuffle 0 (m.Rows - 1) then
                let x = GaussElimination(input, output)
                for i in 0 .. x.Size do
                    for j in [0 .. x.Size] do x.Eliminate(i, j)
                Yes <| x.Output(m.Rows)
            else No
