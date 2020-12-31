functor NipoLexedInput(Lexer: NIPO_LEXER) :> sig
    include NIPO_PARSER_INPUT where type Token.t = Lexer.Token.t

    val tokenize: Lexer.Input.stream -> stream
end = struct
    structure Token = Lexer.Token

    structure Pos = Pos
    
    type stream = {inner: Lexer.Input.stream, buffer: Token.t option ref}
    type positioned = stream

    fun pos {buffer, inner} =
        case !buffer
        of SOME tok => Token.startPos tok
         | NONE => Lexer.Input.pos inner

    fun tokenize inner = {inner = inner, buffer = ref NONE}

    fun tryShift {buffer, inner} = buffer := Lexer.next inner

    fun popBuffer buffer =
        let val res = !buffer
        in buffer := NONE
         ; res
        end

    fun peek (stream as {buffer, inner = _}) =
        case !buffer
        of lookahead as SOME _ => lookahead
         | NONE => (tryShift stream; !buffer)

    fun pop (stream as {buffer, inner = _}) =
        case !buffer
        of SOME _ => popBuffer buffer
         | NONE => (tryShift stream; popBuffer buffer)

    fun inputN _ = raise Fail "unimplemented"
end

