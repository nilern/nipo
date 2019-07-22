structure Lexers = NipoLexers
datatype atom = datatype Lexers.atom

val grammar =
    [ ("token", [ [Char #"|"]
                , [Char #"{"]
                , [Char #"}"] ]) ]

val _ = print (Lexers.lexerCode grammar "token")

