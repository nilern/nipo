structure Lexers = NipoLexers
datatype atom = datatype Grammar.atom
val token = Terminal o SOME

val grammar =
    [ ("token", [ {atoms = [token "#\"=\""], action = SOME "NipoTokens.Eq o #1"}
                , {atoms = [token "#\"|\""], action = SOME "NipoTokens.Bar o #1"}
                , {atoms = [token "#\"{\""], action = SOME "NipoTokens.LBrace o #1"}
                , {atoms = [token "#\"}\""], action = SOME "NipoTokens.RBrace o #1"}
                , {atoms = [token "#\";\""], action = SOME "NipoTokens.Semi o #1"} ])
    , ("ws", [ {atoms = [NonTerminal "wsChar", NonTerminal "ws"], action = NONE}
             , {atoms = [], action = NONE} ])
    , ("wsChar", [ {atoms = [token "#\" \""], action = NONE} ]) ]

val _ = print (Lexers.lexerCode { lexerName = "NipoLexer"
                                , tokenType = "NipoTokens.token"
                                , rules = grammar
                                , startRule = "token"
                                , whitespaceRule = "ws" })

