structure Grammar = Grammar(Token)
structure Parsers = NipoParsers(Grammar)
datatype atom = datatype Grammar.atom

val token = Terminal o SOME

val grammar =
    [ ("parser", [{atoms = [token "Parser"], action = NONE}]) ]

val _ = print (Parsers.parserCode { parserName = "NipoParser"
                                  , tokenType = "NipoTokens.t"
                                  , support = "open NipoTokens"
                                  , grammar = grammar
                                  , startName = "parser" })

