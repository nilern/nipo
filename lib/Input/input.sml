signature NIPO_TOKEN = sig
    type t
    type vector

    val toString: t -> string
    val lookaheadToString: t option -> string
end

signature NIPO_INPUT = sig
    type stream

    structure Token: NIPO_TOKEN

    val peek: stream -> Token.t option
    val pop: stream -> Token.t option
    val inputN: stream * int -> Token.vector
end

signature RESETABLE_NIPO_INPUT = sig
    include NIPO_INPUT

    type checkpoint

    val checkpoint: stream -> checkpoint
    val reset: stream * checkpoint -> unit
end

signature NIPO_LEXER_INPUT = sig
    include RESETABLE_NIPO_INPUT
        where type Token.t = char
        where type Token.vector = string

    include POSITIONED
        where type t = stream

    structure Inner: RESETABLE_NIPO_INPUT
        where type Token.t = Token.t
        where type Token.vector = Token.vector
    val toInner: stream -> Inner.stream

    val fromInner: Inner.stream * Pos.t -> stream
end

