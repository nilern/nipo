structure Lexers = NipoLexers
datatype atom = datatype Grammar.atom
val token = Terminal o SOME

val grammar =
    [ ("token", [ [token "#\"=\""]
                , [token "#\"|\""]
                , [token "#\"{\""]
                , [token "#\"}\""]
                , [token "#\";\""] ]) ]

val _ = print (Lexers.lexerCode "NipoLexer" grammar "token")

