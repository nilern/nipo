signature NIPO_TOKEN = sig
    eqtype t

    val toString: t -> string
end

signature NIPO_INPUT = sig
    type stream
    eqtype token

    structure Token: NIPO_TOKEN where type t = token

    val peek: stream -> token option
    val pop: stream -> token option
end

signature RESETABLE_NIPO_INPUT = sig
    include NIPO_INPUT

    type checkpoint

    val checkpoint: stream -> checkpoint
    val reset: stream * checkpoint -> unit
end

