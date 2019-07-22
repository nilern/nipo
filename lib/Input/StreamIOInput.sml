functor NipoStreamIOInput(Args: sig
    structure IOStream: STREAM_IO
    structure Token: NIPO_TOKEN
        where type t = IOStream.elem
        where type vector = IOStream.vector
end) :> RESETABLE_NIPO_INPUT
    where type Token.t = Args.Token.t
= struct
    structure IOStream = Args.IOStream
    structure Token = Args.Token

    type stream = IOStream.instream ref
    type checkpoint = IOStream.instream

    fun peek (ref ios) = Option.map #1 (IOStream.input1 ios)

    fun pop stream =
        Option.map (fn (token, ios') =>
                        ( stream := ios'
                        ; token ))
                   (IOStream.input1 (!stream))

    fun inputN (stream, n) =
        let val (elems, ios') = IOStream.inputN (!stream, n)
        in stream := ios'
         ; elems
        end

    fun checkpoint (ref ios) = ios

    fun reset (stream, ios) = stream := ios
end

