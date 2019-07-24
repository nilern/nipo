functor NipoLexedInput(Lexer: NIPO_LEXER) :> sig
    include NIPO_INPUT where type Token.t = Lexer.Token.t

    val tokenize: Lexer.Input.stream -> stream
end = struct
    structure Token = Lexer.Token
    
    type stream = {inner: Lexer.Input.stream, buffer: Token.t option ref}

    fun tokenize inner = {inner = inner, buffer = ref NONE}

    fun tryShift {buffer, inner} = buffer := Lexer.next inner

    fun popBuffer buffer =
        let val res = !buffer
        in buffer := NONE
         ; res
        end

    fun peek (stream as {buffer, inner}) =
        case !buffer
        of lookahead as SOME _ => lookahead
         | NONE => (tryShift stream; !buffer)

    fun pop (stream as {buffer, inner}) =
        case !buffer
        of lookahead as SOME _ => popBuffer buffer
         | NONE => (tryShift stream; popBuffer buffer)

    fun inputN _ = raise Fail "unimplemented"
end

