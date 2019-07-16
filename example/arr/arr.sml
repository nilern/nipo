structure Parsers = NipoParsers(NipoStringInput)

val n = Parsers.rule
val t = Parsers.token

val grammar =
    [ ("stmt", [ [n "assign"]
               , [n "expr"] ])
    , ("assign", [[n "lvalue", t #"=", n "expr"]])
    , ("lvalue", [[n "var", t #"[", n "expr", t #"]"]])
    , ("expr", [ [n "var", t #"[", n "expr", t #"]"]
               , [n "const"] ])
    , ("var", [[t #"a"]])
    , ("const", [[t #"0"], [t #"1"]]) ]

val parse = Parsers.parser grammar "stmt"

val _ = parse (ref (VectorSlice.full "a[1]=a[0]"))

