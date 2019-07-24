structure Grammar = Grammar(Token)
structure Parsers = NipoParsers(Grammar)

val grammar =
    [ ("parser", [{atoms = [], action = NONE}]) ]

val _ = print (Parsers.parserCode grammar "parser")

