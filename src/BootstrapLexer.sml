structure Token = CharClass
structure Grammar = LexerGrammar
structure Lookahead = Lookahead(Token)
structure Analysis = GrammarAnalysis(struct
    structure Grammar = Grammar
    structure Analyzed = AnalyzedGrammar(Grammar)
    structure Lookahead = Lookahead
    structure NullableToken = NullableToken(Lookahead)
    structure FirstSet = TokenSet(NullableToken)
    structure FollowSet = FollowSet(struct
        structure Lookahead = Lookahead
        structure NullableToken = NullableToken
        structure FirstSet = FirstSet
    end)
end)
structure Lexers = NipoLexers(struct
    structure Token = Token
    structure Grammar = Grammar
    structure Parsers = NipoParsers(struct
        structure Grammar = Grammar
        structure Lookahead = Lookahead
        structure Analysis = Analysis
    end)
end)
datatype productee = datatype InputGrammar.productee

val charLit = Lit o String.str
val tokens = InSeq o List.map charLit o String.explode
fun charsBetween (first, last) =
    let val firstCode = Char.ord first
        val lastCode = Char.ord last

        fun loop charCode acc =
            if charCode <= lastCode
            then loop (charCode + 1)
                      ({productee = [charLit (Char.chr charCode)], action = NONE} :: acc)
            else List.rev acc
    in loop firstCode []
    end

val grammar =
    [ ("token", [ {productee = Var "id", action = SOME "NipoTokens.fromId"}
                , { productee = Var "escapedId"
                  , action = SOME "fn (s, cs, e) => NipoTokens.Lit (s, String.substring (cs, 1, String.size cs - 2), e)" }
                , {productee = tokens "->", action = SOME "NipoTokens.Arrow o #1"}
                , {productee = Lit "=", action = SOME "NipoTokens.Eq o #1"}
                , {productee = Lit "|", action = SOME "NipoTokens.Bar o #1"}
                , {productee = Lit "?", action = SOME "NipoTokens.QMark o #1"}
                , {productee = Lit "*", action = SOME "NipoTokens.Star o #1"}
                , {productee = Lit "+", action = SOME "NipoTokens.Plus o #1"}
                , { productee = InSeq [Lit "{", Var "action", Lit "}"]
                  , action = SOME "fn (s, cs, e) => NipoTokens.Action (s, String.substring (cs, 1, String.size cs - 2), e)" }
                , { productee = InSeq [Lit "[", Lit "[", Lit ":", Var "posix", Lit ":", Lit "]", Lit "]"],
                    action = SOME "fn (s, cs, e) => NipoTokens.Posix (s, String.substring (cs, 3, String.size cs - 6), e)" }
                , {productee = Lit "(", action = SOME "NipoTokens.LParen o #1"}
                , {productee = Lit ")", action = SOME "NipoTokens.RParen o #1"}
                , {productee = Lit ";", action = SOME "NipoTokens.Semi o #1"} ])
    , ("id", [{productee = InSeq [Var "alpha", Var "idTail"], action = NONE}])
    , ("idTail", [ {productee = InSeq [Var "alpha", Var "idTail"], action = NONE}
                 , {productee = InSeq [], action = NONE} ])
    , ("escapedId", [{productee = InSeq [Lit "'", Var "freeIdContents", Lit "'"], action = NONE}])
    , ("freeIdContents", [ {productee = InSeq [Complement (Lit "'"), Var "freeIdContents"], action = NONE}
                         , {productee = InSeq [], action = NONE} ])
    , ("alpha", [ {productee = Posix "alpha", action = NONE} ])
    , ("posix", [ {productee = Var "id", action = NONE} ])
    , ("action", [ {productee = InSeq [Complement (Lit "}"), Var "action"], action = NONE}
                 , {productee = InSeq [], action = NONE} ])
    , ("ws", [ {productee = InSeq [ InAlt [ {productee = Var "wsChar", action = NONE}
                                          , {productee = Var "comment", action = NONE} ]
                                  , Var "ws"], action = NONE}
             , {productee = InSeq [], action = NONE} ])
    , ("wsChar", [{productee = Posix "space", action = NONE}])
    , ("comment", [{productee = InSeq [Lit "#", InMany (Complement (Lit "\\n"))], action = NONE}]) ]

val _ = print (Lexers.lexerCode { lexerName = "NipoLexer"
                                , tokenType = "NipoTokens.token"
                                , support = ""
                                , rules = grammar
                                , startRule = "token"
                                , whitespaceRule = "ws" })

