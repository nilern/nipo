signature NIPO_LEXER = sig
    structure Input: NIPO_LEXER_INPUT

    val next: Input.stream -> Input.Token.t option
end

