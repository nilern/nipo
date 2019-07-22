signature NIPO_LEXER = sig
    structure Input: NIPO_LEXER_INPUT

    type token

    val next: Input.stream -> token option
end

