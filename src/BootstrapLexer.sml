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
datatype atom = datatype InputGrammar.atom

val charLit = Lit o String.str
val tokens = List.map charLit o String.explode
fun charsBetween (first, last) =
    let val firstCode = Char.ord first
        val lastCode = Char.ord last

        fun loop charCode acc =
            if charCode <= lastCode
            then loop (charCode + 1)
                      ({atoms = [charLit (Char.chr charCode)], action = NONE} :: acc)
            else List.rev acc
    in loop firstCode []
    end

val grammar =
    [ ("token", [ {atoms = [Var "id"], action = SOME "NipoTokens.fromId"}
                , { atoms = [Var "escapedId"]
                  , action = SOME "fn (s, cs, e) => NipoTokens.Lit (s, String.substring (cs, 1, String.size cs - 2), e)" }
                , {atoms = tokens "->", action = SOME "NipoTokens.Arrow o #1"}
                , {atoms = [Lit "="], action = SOME "NipoTokens.Eq o #1"}
                , {atoms = [Lit "|"], action = SOME "NipoTokens.Bar o #1"}
                , { atoms = [Lit "{", Var "action", Lit "}"]
                  , action = SOME "fn (s, cs, e) => NipoTokens.Action (s, String.substring (cs, 1, String.size cs - 2), e)" }
                , { atoms = [Lit "[", Lit "[", Lit ":", Var "posix", Lit ":", Lit "]", Lit "]"],
                    action = SOME "fn (s, cs, e) => NipoTokens.Posix (s, String.substring (cs, 3, String.size cs - 6), e)" }
                , {atoms = [Lit ";"], action = SOME "NipoTokens.Semi o #1"} ])
    , ("id", [{atoms = [Var "alpha", Var "idTail"], action = NONE}])
    , ("idTail", [ {atoms = [Var "alpha", Var "idTail"], action = NONE}
                 , {atoms = [], action = NONE} ])
    , ("escapedId", [{atoms = [Lit "'", Var "freeIdContents", Lit "'"], action = NONE}])
    , ("freeIdContents", [ {atoms = [Complement (Lit "'"), Var "freeIdContents"], action = NONE}
                         , {atoms = [], action = NONE} ])
    , ("alpha", [ {atoms = [Posix "alpha"], action = NONE} ])
    , ("posix", [ {atoms = [Var "id"], action = NONE} ])
    , ("action", [ {atoms = [Complement (Lit "}"), Var "action"], action = NONE}
                 , {atoms = [], action = NONE} ])
    , ("ws", [ {atoms = [Var "wsChar", Var "ws"], action = NONE}
             , {atoms = [], action = NONE} ])
    , ("wsChar", [ {atoms = [Lit " "], action = NONE}
                 , {atoms = [Lit "\\t"], action = NONE}
                 , {atoms = [Lit "\\r"], action = NONE}
                 , {atoms = [Lit "\\n"], action = NONE}]) ]

val _ = print (Lexers.lexerCode { lexerName = "NipoLexer"
                                , tokenType = "NipoTokens.token"
                                , rules = grammar
                                , startRule = "token"
                                , whitespaceRule = "ws" })

