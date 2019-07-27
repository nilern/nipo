structure TextIOInput = NipoStreamIOInput(struct
    structure IOStream = TextIO.StreamIO
    structure Token = CharToken
end)
structure LexerTextInput = LexerInput(TextIOInput)
structure Lexer = ArithLexer(struct
    structure Input = LexerTextInput
    structure Token = ArithToken
end)

structure TokenStream = NipoLexedInput(Lexer)
structure Parser = ArithParser(TokenStream)

fun main () =
    let val input = TextIO.getInstream TextIO.stdIn
        val input = TextIOInput.fromInstream input
        val input = LexerTextInput.fromInner (input, Pos.default "<stdin>")
        val tokens = TokenStream.tokenize input
        val res = Parser.start__expr tokens
    in print (Int.toString res ^ "\n")
    end

val _ = main ()

