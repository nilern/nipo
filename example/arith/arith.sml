structure Parsers = NipoParsers(NipoStringInput)

val n = Parsers.rule
val t = Parsers.token

val grammar =
    [ ("expr", [[n "term", n "terms"]])
    , ("terms", [ [t #"+", n "term", n "terms"]
                , [] ])
    , ("term", [[n "factor", n "factors"]])
    , ("factors", [ [t #"*", n "factor", n "factors"]
                  , [] ])
    , ("factor", [ [t #"(", n "expr", t #")"]
                 , [n "digit"] ])
    , ("digit", [ [t #"0"], [t #"1"], [t #"2"]
                , [t #"3"], [t #"4"], [t #"5"]
                , [t #"6"], [t #"7"], [t #"8"], [t #"9"] ]) ]

val parse = Parsers.parser grammar "expr"

val _ = print (Parsers.parserCode grammar "expr" ^ "\n")

val _ = parse (ref (VectorSlice.full "(1+2)*3"))

