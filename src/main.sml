structure TextIOInput = NipoStreamIOInput(struct
    structure IOStream = TextIO.StreamIO

    structure Token = struct
        type t = char
        type vector = string

        val toString = Char.toString

        val lookaheadToString =
            fn SOME c => toString c
             | NONE => "EOF"
    end
end)
structure LexerTextInput = LexerInput(TextIOInput)
structure Lexer = NipoLexer(struct
    structure Input = LexerTextInput
    structure Token = NipoTokens
end)

structure TokenStream = NipoLexedInput(Lexer)

val main =
    fn [filename] =>
        let fun lexAll tokens =
                case TokenStream.pop tokens
                of SOME token => 
                    ( print (NipoTokens.toString token ^ "\n")
                    ; lexAll tokens )
                 | NONE => ()
            val input = TextIO.getInstream (TextIO.openIn filename)
            val lexerInput = LexerTextInput.fromInner (TextIOInput.fromInstream input, Pos.default "CLI")
            val tokens = TokenStream.tokenize lexerInput
        in lexAll tokens
        end

do main (CommandLine.arguments ())

