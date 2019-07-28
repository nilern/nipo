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
val tokens = Seq o List.map charLit o String.explode
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
                , { productee = Seq [Lit "{", Var "action", Lit "}"]
                  , action = SOME "fn (s, cs, e) => NipoTokens.Action (s, String.substring (cs, 1, String.size cs - 2), e)" }
                , { productee = Seq [Lit "[", Lit "[", Lit ":", Var "posix", Lit ":", Lit "]", Lit "]"],
                    action = SOME "fn (s, cs, e) => NipoTokens.Posix (s, String.substring (cs, 3, String.size cs - 6), e)" }
                , {productee = Lit ";", action = SOME "NipoTokens.Semi o #1"} ])
    , ("id", [{productee = Seq [Var "alpha", Var "idTail"], action = NONE}])
    , ("idTail", [ {productee = Seq [Var "alpha", Var "idTail"], action = NONE}
                 , {productee = Seq [], action = NONE} ])
    , ("escapedId", [{productee = Seq [Lit "'", Var "freeIdContents", Lit "'"], action = NONE}])
    , ("freeIdContents", [ {productee = Seq [Complement (Lit "'"), Var "freeIdContents"], action = NONE}
                         , {productee = Seq [], action = NONE} ])
    , ("alpha", [ {productee = Posix "alpha", action = NONE} ])
    , ("posix", [ {productee = Var "id", action = NONE} ])
    , ("action", [ {productee = Seq [Complement (Lit "}"), Var "action"], action = NONE}
                 , {productee = Seq [], action = NONE} ])
    , ("ws", [ {productee = Seq [Var "wsChar", Var "ws"], action = NONE}
             , {productee = Seq [], action = NONE} ])
    , ("wsChar", [ {productee = Lit " ", action = NONE}
                 , {productee = Lit "\\t", action = NONE}
                 , {productee = Lit "\\r", action = NONE}
                 , {productee = Lit "\\n", action = NONE}]) ]

val _ = print (Lexers.lexerCode { lexerName = "NipoLexer"
                                , tokenType = "NipoTokens.token"
                                , rules = grammar
                                , startRule = "token"
                                , whitespaceRule = "ws" })

