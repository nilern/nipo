signature LEXEME = sig
    type t

    val compare: t * t -> order
    val overlap: t * t -> bool
    val toString: t -> string
end
