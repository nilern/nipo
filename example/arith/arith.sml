structure Parsers = NipoParsers(NipoStringInput)

val n = Parsers.rule
val t = Parsers.token
val op<*> = Parsers.<*>
val op<|> = Parsers.<|>

(* TODO: Left recursion elimination. *)
val grammar =
    [ ("expr", n "expr" <*> t #"+" <*> n "term"
           <|> n "term")
    , ("term", n "term" <*> t #"*" <*> n "factor"
           <|> n "factor")
    , ("factor", t #"(" <*> n "expr" <*> t #")"
             <|> n "digit")
    , ("digit", t #"0" <|> t #"1" <|> t #"2"
            <|> t #"3" <|> t #"4" <|> t #"5"
            <|> t #"6" <|> t #"7" <|> t #"8" <|> t #"9") ]

val parse = Parsers.parser grammar "expr"

val _ = parse (ref (VectorSlice.full "(1+2)*3"))

