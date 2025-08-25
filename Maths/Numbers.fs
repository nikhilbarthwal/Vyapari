namespace Vyapari.Maths

open System.Numerics


type Fraction private (n0: bigint, d0: bigint) =
    let n, d =
        assert (d0 <> 0I)
        let n1, d1 = if (d0 < 0I) then ((-1I * n0), (-1I * d0)) else (n0, d0)
        let g = BigInteger.GreatestCommonDivisor(n1, d1)
        n1/g, d1/g

    member this.N = n
    member this.D = d

    new (ni: int) = Fraction(bigint(ni), 1I)
    new (ni: int, di: int) = Fraction(bigint(ni), bigint(di))
    new (db: float) =
        Fraction(bigint (db * 1000.0 * 1000.0 * 1000.0), 1000I * 1000I * 1000I)

    member this.ToFloat() =
        let db = float((n * 1000I * 1000I * 1000I) / d)
        db / (1000.0 * 1000.0 * 1000.0)

    member this.Pow(power: int) =
        Fraction(BigInteger.Pow(this.N, power), BigInteger.Pow(this.D, power))

    member this.IsOne() : bool = this.N = this.D
    member this.IsZero() : bool = this.N = 0I

    override this.ToString() = let db = this.ToFloat() in $"%.6f{db}"

    static member (+) (n1: Fraction, n2: Fraction) =
        Fraction(n1.N * n2.D + n1.D * n2.N, n1.D * n2.D)

    static member (-) (n1: Fraction, n2: Fraction) =
        Fraction(n1.N * n2.D - n1.D * n2.N, n1.D * n2.D)

    static member (*) (n1: Fraction, n2: Fraction) =
        Fraction(n1.N * n2.N, n1.D * n2.D)

    static member (/) (n1: Fraction, n2: Fraction) =
        Fraction(n1.N * n2.D, n1.D * n2.N)

    interface IAdditiveIdentity<Fraction, Fraction> with
        static member AdditiveIdentity = Fraction(0)

    interface IMultiplicativeIdentity<Fraction, Fraction> with
        static member MultiplicativeIdentity = Fraction(1)

    interface IAdditionOperators<Fraction, Fraction, Fraction> with
        static member (+) (n1: Fraction, n2: Fraction) = n1 + n2

    interface ISubtractionOperators<Fraction, Fraction, Fraction> with
        static member (-) (n1: Fraction, n2: Fraction) = n1 - n2

    interface IMultiplyOperators<Fraction, Fraction, Fraction> with
        static member (*) (n1: Fraction, n2: Fraction) = n1 * n2

    interface IDivisionOperators<Fraction, Fraction, Fraction> with
        static member (/) (n1: Fraction, n2: Fraction) = n1 / n2

    interface System.IComparable<Fraction> with
        override this.CompareTo(f: Fraction) =
            let n = this.N*f.D in let d = this.D * f.N in n.CompareTo(d)

    interface System.IEquatable<Fraction> with
        override this.Equals(f: Fraction) =
            let n = this.N*f.D in let d = this.D * f.N in n.Equals(d)


type Complex(real: float, imaginary: float) =
    member this.Real = real
    member this.Imaginary = imaginary
    member this.Magnitude() = real * real + imaginary * imaginary

    static member Arc (p: int, q: int): Complex =
         let pi = 3.141593
         let phase = 2.0 * pi * (float p) / (float q)
         Complex(System.Math.Cos phase, System.Math.Sin phase)

    static member (+) (c1: Complex, c2: Complex) =
        Complex(c1.Real + c2.Real, c1.Imaginary + c2.Imaginary)

    static member (-) (c1: Complex, c2: Complex) =
        Complex(c1.Real - c2.Real, c1.Imaginary - c2.Imaginary)

    static member (*) (c1: Complex, c2: Complex) =
        let real = c1.Real * c2.Real - c1.Imaginary * c2.Imaginary
        let imaginary = c1.Real * c2.Imaginary + c1.Imaginary * c2.Real
        Complex(real, imaginary)

    static member (/) (c: Complex, f: float) = Complex(c.Real/f, c.Imaginary/f)

    static member (/) (f: float, c: Complex) = Complex(c.Real/f, c.Imaginary/f)

    static member (/) (c1: Complex, c2: Complex) =
        let r = c1.Real* c2.Real + c1.Imaginary * c2.Imaginary
        let i = c2.Real * c1.Imaginary - c1.Real * c2.Imaginary
        let d = c2.Real * c2.Real + c1.Imaginary * c1.Imaginary
        Complex(r/d, i/d)

    interface IAdditiveIdentity<Complex, Complex> with
        static member AdditiveIdentity = Complex(0, 0)

    interface IMultiplicativeIdentity<Complex, Complex> with
        static member MultiplicativeIdentity = Complex(1, 0)

    interface IAdditionOperators<Complex, Complex, Complex> with
        static member (+) (c1: Complex, c2: Complex) = c1 + c2

    interface ISubtractionOperators<Complex, Complex, Complex> with
        static member (-) (c1: Complex, c2: Complex) = c1 - c2

    interface IMultiplyOperators<Complex, Complex, Complex> with
        static member (*) (c1: Complex, c2: Complex)= c1 * c2

    interface IDivisionOperators<Complex, Complex, Complex> with
        static member (/)  (c1: Complex, c2: Complex) = c1 / c2

    interface System.IEquatable<Complex> with
        override this.Equals(x: Complex) =
            this.Real.Equals(x.Real) && this.Imaginary.Equals(x.Imaginary)

    override this.ToString() = $"{this.Real} + {this.Imaginary}i"
