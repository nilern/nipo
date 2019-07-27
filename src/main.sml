structure TextIOInput = NipoStreamIOInput(struct
    structure IOStream = TextIO.StreamIO
    structure Token = CharToken
end)
structure LexerTextInput = LexerInput(TextIOInput)
structure Lexer = NipoLexer(struct
    structure Input = LexerTextInput
    structure Token = NipoTokens
end)

structure TokenStream = NipoLexedInput(Lexer)
structure Parser = NipoParser(TokenStream)

structure LexerLookahead = Lookahead(CharClass)
structure LexerAnalysis = GrammarAnalysis(struct
    structure Grammar = LexerGrammar
    structure Analyzed = AnalyzedGrammar(Grammar)
    structure Lookahead = LexerLookahead
    structure NullableToken = NullableToken(Lookahead)
    structure FirstSet = TokenSet(NullableToken)
    structure FollowSet = FollowSet(struct
        structure Lookahead = Lookahead
        structure NullableToken = NullableToken
        structure FirstSet = FirstSet
    end)
end)
structure Lexers = NipoLexers(struct
    structure Token = CharClass
    structure Grammar = LexerGrammar
    structure Parsers = NipoParsers(struct
        structure Grammar = Grammar
        structure Lookahead = LexerLookahead
        structure Analysis = LexerAnalysis
    end)
end)

structure ParserLookahead = Lookahead(ParserGrammar.Token)
structure ParserAnalysis = GrammarAnalysis(struct
    structure Grammar = ParserGrammar
    structure Analyzed = AnalyzedGrammar(Grammar)
    structure Lookahead = ParserLookahead
    structure NullableToken = NullableToken(Lookahead)
    structure FirstSet = TokenSet(NullableToken)
    structure FollowSet = FollowSet(struct
        structure Lookahead = Lookahead
        structure NullableToken = NullableToken
        structure FirstSet = FirstSet
    end)
end)
structure ProperParsers = ProperParsers(struct
    structure Grammar = ParserGrammar
    structure Lookahead = ParserLookahead
    structure Analysis = ParserAnalysis
end)

val parserCode =
    fn InputGrammar.Lexer lexer => Lexers.lexerCode lexer
     | InputGrammar.Parser parser => ProperParsers.parserCode parser

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
        in print (parserCode (Parser.start__parser tokens))
        end

do main (CommandLine.arguments ())

