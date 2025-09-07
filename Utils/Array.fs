namespace Vyapari

type Array<'T> = abstract member Item: int -> 'T
                 abstract member Length: int
                 abstract member Get: int -> 'T

module Array =

    type Buffer<'T>(length: int, gen: int -> 'T) =
        let data = [| for i in 0 .. length - 1 -> gen i |]
        member this.Overwrite(f: int -> 'T) =
            for i in 0 .. length - 1 do data[i] <- f i

        member this.Length = length
        member this.Swap(i, j) =
            let t = data[i] in (data[i] <- data[j] ; data[j] <- t)

        member this.Item
            with get(index: int) =  data[index]
            and set(index: int) (value: 'T) = data[index] <- value

        member this.Get(index) = data[index]

        interface Array<'T> with
            member this.Item(index: int) = data[index]
            member this.Length = length
            member this.Get(index) = data[index]

    let Random (length: int) (min: int, max: int): Array<int> =
        let random = System.Random(System.Guid.NewGuid().GetHashCode())
        Buffer(length, fun _ ->  random.Next(min, max))

    let Initialize<'T>(length: int, gen: int -> 'T): Array<'T> = Buffer(length, gen)
