signature LEXEME = sig
    type t

    val compare: t * t -> order
    val toString: t -> string
end
