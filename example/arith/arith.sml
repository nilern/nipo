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
        
        fun lexAll tokens =
            case TokenStream.pop tokens
            of SOME token =>
                ( print (ArithToken.toString token ^ "\n")
                ; lexAll tokens )
             | NONE => ()
    in lexAll (TokenStream.tokenize input)
    end

val _ = main ()

