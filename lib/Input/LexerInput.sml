functor LexerInput(Inner: RESETABLE_NIPO_INPUT
    where type Token.t = char
    where type Token.vector = string
) :> NIPO_LEXER_INPUT
    where type Inner.stream = Inner.stream
= struct
    structure Pos = Pos

    type stream = {inner: Inner.stream, pos: Pos.t ref}
    type positioned = stream
    type checkpoint = {inner: Inner.checkpoint, pos: Pos.t}

    structure Token = Inner.Token

    structure Inner = Inner

    fun fromInner (inner, pos) = {inner = inner, pos = ref pos}
    
    val toInner: stream -> Inner.stream = #inner

    val peek: stream -> Token.t option = Inner.peek o #inner

    fun pop {inner = inner, pos = pos} =
        Option.map (fn c =>
                        ( pos := Pos.advance (c, !pos)
                        ; c ))
                   (Inner.pop inner)

    fun inputN ({inner = inner, pos = pos}, n) =
        let val res = Inner.inputN (inner, n)
        in pos := Vector.foldl Pos.advance (!pos) res
         ; res
        end

    fun checkpoint {inner = inner, pos = ref pos} =
        { inner = Inner.checkpoint inner
        , pos = pos }

    fun reset ({inner = inner, pos = pos}, checkpoint: checkpoint) =
        ( Inner.reset (inner, #inner checkpoint)
        ; pos := #pos checkpoint )

    fun pos {inner = _, pos = ref pos} = pos
end

