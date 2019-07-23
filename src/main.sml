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
structure Lexer = NipoLexer(LexerTextInput)

val main =
    fn [filename] =>
        let fun lexAll input =
                case Lexer.next input
                of SOME token => 
                    ( print (NipoTokens.toString token ^ "\n")
                    ; lexAll input )
                 | NONE => ()
            val input = TextIO.getInstream (TextIO.openIn filename)
        in lexAll (LexerTextInput.fromInner (TextIOInput.fromInstream input, Pos.default "CLI"))
        end

do main (CommandLine.arguments ())

