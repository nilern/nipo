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
                , {atoms = [token #"="], action = SOME "NipoTokens.Eq o #1"}
                , {atoms = [token #"|"], action = SOME "NipoTokens.Bar o #1"}
                , {atoms = [token #"{"], action = SOME "NipoTokens.LBrace o #1"}
                , {atoms = [token #"}"], action = SOME "NipoTokens.RBrace o #1"}
                , {atoms = [token #";"], action = SOME "NipoTokens.Semi o #1"} ])
    , ("id", [{atoms = [NonTerminal "alpha", NonTerminal "idTail"], action = NONE}])
    , ("idTail", [ {atoms = [NonTerminal "alpha", NonTerminal "idTail"], action = NONE}
                 , {atoms = [], action = NONE} ])
    , ("alpha", charsBetween (#"a", #"z") @ charsBetween (#"A", #"Z")) (* HACK: ASCII *)
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

