signature NIPO_LEXER = sig
    structure Input: NIPO_LEXER_INPUT
    structure Token: NIPO_POSITIONED_TOKEN

    val next: Input.stream -> Token.t option
end

