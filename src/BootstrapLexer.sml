structure Lexers = NipoLexers
datatype atom = datatype Grammar.atom
val token = Terminal o SOME

val grammar =
    [ ("token", [ {atoms = [token "#\"=\""], action = "NipoTokens.Eq o #1"}
                , {atoms = [token "#\"|\""], action = "NipoTokens.Bar o #1"}
                , {atoms = [token "#\"{\""], action = "NipoTokens.LBrace o #1"}
                , {atoms = [token "#\"}\""], action = "NipoTokens.RBrace o #1"}
                , {atoms = [token "#\";\""], action = "NipoTokens.Semi o #1"} ]) ]

val _ = print (Lexers.lexerCode "NipoLexer" "NipoTokens.token" grammar "token")

