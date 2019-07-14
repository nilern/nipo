structure Parsers = NipoParsers(NipoStringInput)

val n = Parsers.rule
val t = Parsers.token
val op<*> = Parsers.<*>
val op<|> = Parsers.<|>

val grammar =
    [ ("as", t #"a" <*> n "as"
         <|> t #"A") ]

val parse = Parsers.parser grammar "as"

val _ = parse (ref (VectorSlice.full "aaaA"))

