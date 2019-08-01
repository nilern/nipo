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

fun printErrLine s = TextIO.output (TextIO.stdErr, s ^ "\n")

val pos = Pos.default "BootstrapLexer.sml"
fun nonTerminal name = {pos, v = Var name}
fun token cs = {pos, v = Lit cs}
fun complement inner = {pos, v = Complement inner}
fun alt ps = {pos, v = InAlt ps}
fun seq ps = {pos, v = InSeq ps}
fun charLit c = {pos, v = Lit (String.str c)}
fun tokens cs = {pos, v = InSeq (List.map charLit (String.explode cs))}
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
    [ ("token", [ {productee = nonTerminal "id", action = SOME "NipoTokens.fromId"}
                , { productee = nonTerminal "escapedId"
                  , action = SOME "fn (s, cs, e) => NipoTokens.Lit (s, String.substring (cs, 1, String.size cs - 2), e)" }
                , {productee = tokens "->", action = SOME "NipoTokens.Arrow o #1"}
                , {productee = token "=", action = SOME "NipoTokens.Eq o #1"}
                , {productee = token "|", action = SOME "NipoTokens.Bar o #1"}
                , {productee = token "?", action = SOME "NipoTokens.QMark o #1"}
                , {productee = token "*", action = SOME "NipoTokens.Star o #1"}
                , {productee = token "+", action = SOME "NipoTokens.Plus o #1"}
                , { productee = seq [token "{", nonTerminal "action", token "}"]
                  , action = SOME "fn (s, cs, e) => NipoTokens.Action (s, String.substring (cs, 1, String.size cs - 2), e)" }
                , { productee = seq [token "[", token "[", token ":", nonTerminal "posix", token ":", token "]", token "]"],
                    action = SOME "fn (s, cs, e) => NipoTokens.Posix (s, String.substring (cs, 3, String.size cs - 6), e)" }
                , {productee = token "(", action = SOME "NipoTokens.LParen o #1"}
                , {productee = token ")", action = SOME "NipoTokens.RParen o #1"}
                , {productee = token ";", action = SOME "NipoTokens.Semi o #1"} ])
    , ("id", [{productee = seq [nonTerminal "alpha", nonTerminal "idTail"], action = NONE}])
    , ("idTail", [ {productee = seq [nonTerminal "alpha", nonTerminal "idTail"], action = NONE}
                 , {productee = seq [], action = NONE} ])
    , ("escapedId", [{productee = seq [token "'", nonTerminal "freeIdContents", token "'"], action = NONE}])
    , ("freeIdContents", [ {productee = seq [complement (token "'"), nonTerminal "freeIdContents"], action = NONE}
                         , {productee = seq [], action = NONE} ])
    , ("alpha", [ {productee = {pos, v = Posix "alpha"}, action = NONE} ])
    , ("posix", [ {productee = nonTerminal "id", action = NONE} ])
    , ("action", [ {productee = seq [complement (token "}"), nonTerminal "action"], action = NONE}
                 , {productee = seq [], action = NONE} ])
    , ("ws", [ {productee = seq [ alt [ {productee = nonTerminal "wsChar", action = NONE}
                                      , {productee = nonTerminal "comment", action = NONE} ]
                                , nonTerminal "ws"], action = NONE}
             , {productee = seq [], action = NONE} ])
    , ("wsChar", [{productee = {pos, v = Posix "space"}, action = NONE}])
    , ("comment", [{ productee = seq [token "#", {pos, v = InMany (complement (token "\\n"))}, token "\\n"]
                   , action = NONE }]) ]

open OS.Process
val _ =
    ( print (Lexers.lexerCode { lexerName = "NipoLexer"
                                , tokenType = "NipoTokens.token"
                                , support = ""
                                , rules = grammar
                                , startRule = "token"
                                , whitespaceRule = "ws" })
    ; exit success )
    handle
        | Analysis.Conflicts conflicts =>
            ( printErrLine (Analysis.formatConflicts conflicts)
            ; exit failure )

