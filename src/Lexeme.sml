signature LEXEME = sig
    type t

    val compare: t * t -> order
    val overlap: t * t -> bool
    val toString: t -> string
end

structure Token = struct
    type t = string

    fun toString token = token

    val compare = String.compare

    val overlap = op=
end

