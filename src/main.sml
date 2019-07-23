structure LexerStringInput = LexerInput(NipoStringInput)
structure Lexer = NipoLexer(LexerStringInput)

val main =
    fn [input] =>
        let fun lexAll input =
                case Lexer.next input
                of SOME token => 
                    ( print (NipoTokens.toString token ^ "\n")
                    ; lexAll input )
                 | NONE => ()
        in lexAll (LexerStringInput.fromInner (NipoStringInput.fromString input, Pos.default "CLI"))
        end

do main (CommandLine.arguments ())

