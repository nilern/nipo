structure Lexers = NipoLexers
structure Grammar = Lexers.Grammar
datatype atom = datatype Grammar.atom

val token = Terminal o SOME o CharClass.Singleton
val tokens = List.map token o String.explode
fun charsBetween (first, last) =
    let val firstCode = Char.ord first
        val lastCode = Char.ord last

        fun loop charCode acc =
            if charCode <= lastCode
            then loop (charCode + 1)
                      ({atoms = [token (Char.chr charCode)], action = NONE} :: acc)
            else List.rev acc
    in loop firstCode []
    end

val grammar =
    [ ("token", [ {atoms = tokens "lexer", action = SOME "NipoTokens.Lexer o #1"}
                , {atoms = tokens "where", action = SOME "NipoTokens.Where o #1"}
                , {atoms = tokens "rules", action = SOME "NipoTokens.Rules o #1"}
                , {atoms = tokens "start", action = SOME "NipoTokens.Start o #1"}
                , {atoms = [NonTerminal "id"], action = SOME "NipoTokens.Id"}
                , { atoms = [NonTerminal "escapedId"]
                  , action = SOME "fn (s, cs, e) => NipoTokens.Id (s, String.substring (cs, 1, String.size cs - 2), e)" }
                , {atoms = [token #"="], action = SOME "NipoTokens.Eq o #1"}
                , {atoms = [token #"|"], action = SOME "NipoTokens.Bar o #1"}
                , {atoms = [token #"{", NonTerminal "action", token #"}"]
                  , action = SOME "fn (s, cs, e) => NipoTokens.Action (s, String.substring (cs, 1, String.size cs - 2), e)" }
                , {atoms = [token #";"], action = SOME "NipoTokens.Semi o #1"} ])
    , ("id", [{atoms = [NonTerminal "alpha", NonTerminal "idTail"], action = NONE}])
    , ("idTail", [ {atoms = [NonTerminal "alpha", NonTerminal "idTail"], action = NONE}
                 , {atoms = [], action = NONE} ])
    , ("escapedId", [{atoms = [token #"'", NonTerminal "freeIdContents", token #"'"], action = NONE}])
    , ("freeIdContents", [ {atoms = [ Terminal (SOME (CharClass.Not (CharClass.Singleton #"'")))
                                    , NonTerminal "freeIdContents" ], action = NONE}
                         , {atoms = [], action = NONE} ])
    , ("alpha", [ {atoms = [Terminal (SOME (CharClass.Posix CharClass.Alpha))], action = NONE} ])
    , ("action", [ {atoms = [ Terminal (SOME (CharClass.Not (CharClass.Singleton #"}")))
                            , NonTerminal "action" ], action = NONE}
                 , {atoms = [], action = NONE} ])
    , ("ws", [ {atoms = [NonTerminal "wsChar", NonTerminal "ws"], action = NONE}
             , {atoms = [], action = NONE} ])
    , ("wsChar", [ {atoms = [token #" "], action = NONE}
                 , {atoms = [token #"\t"], action = NONE}
                 , {atoms = [token #"\r"], action = NONE}
                 , {atoms = [token #"\n"], action = NONE}]) ]

val _ = print (Lexers.lexerCode { lexerName = "NipoLexer"
                                , tokenType = "NipoTokens.token"
                                , rules = grammar
                                , startRule = "token"
                                , whitespaceRule = "ws" })

