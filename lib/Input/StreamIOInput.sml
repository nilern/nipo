functor NipoStreamIOInput(IOStream: STREAM_IO) :> RESETABLE_NIPO_INPUT
    where type token = IOStream.elem
= struct
    type stream = IOStream.instream ref
    type token = IOStream.elem
    type checkpoint = IOStream.instream

    fun peek (ref ios) = Option.map #1 (IOStream.input1 ios)

    fun pop stream =
        Option.map (fn (token, ios') =>
                        ( stream := ios'
                        ; token ))
                   (IOStream.input1 (!stream))

    fun checkpoint (ref ios) = ios

    fun reset (stream, ios) = stream := ios
end

