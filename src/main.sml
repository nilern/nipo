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
structure Parser = NipoParser(TokenStream)

val main =
    fn [filename] =>
        let fun lexAll tokens =
                case TokenStream.pop tokens
                of SOME token => 
                    ( print (NipoTokens.toString token ^ "\n")
                    ; lexAll tokens )
                 | NONE => ()
            val input = TextIO.getInstream (TextIO.openIn filename)
            val lexerInput = LexerTextInput.fromInner (TextIOInput.fromInstream input, Pos.default filename)
            val tokens = TokenStream.tokenize lexerInput
            do lexAll tokens
            val lexerInput = LexerTextInput.fromInner (TextIOInput.fromInstream input, Pos.default filename)
            val tokens = TokenStream.tokenize lexerInput
        in Parser.start__parser tokens
         ; ()
        end

do main (CommandLine.arguments ())

