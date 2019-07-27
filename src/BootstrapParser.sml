structure Grammar = ParserGrammar
structure Lookahead = Lookahead(Grammar.Token)
structure Analysis = GrammarAnalysis(struct
    structure Grammar = Grammar
    structure Analyzed = AnalyzedGrammar(Grammar)
    structure Lookahead = Lookahead
    structure NullableToken = NullableToken(Lookahead)
    structure FirstSet = TokenSet(NullableToken)
    structure FollowSet = FollowSet(struct
        structure Lookahead = Lookahead
        structure NullableToken = NullableToken
        structure FirstSet = FirstSet
    end)
end)
structure Parsers = ProperParsers(struct
    structure Grammar = Grammar
    structure Lookahead = Lookahead
    structure Analysis = Analysis
end)
datatype atom = datatype Grammar.atom

val token = Terminal o SOME
fun namedNt name = Named (name, NonTerminal name)

val grammar =
    [ ("parser", [ {atoms = [Named ("parser", NonTerminal "properParser")], action = SOME "InputGrammar.Parser parser"}
                 , {atoms = [Named ("lexer", NonTerminal "lexer")], action = SOME "InputGrammar.Lexer lexer"} ])
    , ("properParser", [{ atoms = [ token "Parser", Named ("parserName", token "Id"), token "Where"
                                  , token "Rules", namedNt "rules" ]
                        , action = SOME ( "{parserName = tokenChars parserName, rules = #rules rules, startRule = #startRule rules"
                                        ^ ", support = \"\", tokenCtors = [], tokenType = \"\"}" ) }])
    , ("lexer", [{ atoms = [ token "Lexer", Named ("lexerName", token "Id"), token "Where"
                           , token "Rules", Named ("rules", NonTerminal "rules") ]
                 , action = SOME ( "{lexerName = tokenChars lexerName, rules = #rules rules, startRule = #startRule rules"
                                 ^ ", tokenType = \"\", whitespaceRule = \"\"}" ) }])
    , ("rules", [{ atoms = [ Named ("starter", NonTerminal "startRule")
                           , Named ("others", NonTerminal "auxRules") ]
                 , action = SOME "{startRule = #1 starter, rules = starter :: others}" }])
    , ("auxRules", [ { atoms = [Named ("rule", NonTerminal "rule"), Named ("rules", NonTerminal "auxRules")]
                     , action = SOME "rule :: rules" }
                   , { atoms = [], action = SOME "[]" } ])
    , ("startRule", [{atoms = [token "Start", Named ("rule", NonTerminal "rule")], action = SOME "rule"}])
    , ("rule", [{ atoms = [Named ("name", token "Id"), token "Eq", Named ("productees", NonTerminal "productees"), token "Semi"]
                , action = SOME "(tokenChars name, productees)" }])
    , ("productees", [ { atoms = [Named ("productee", NonTerminal "productee"), Named ("productees", NonTerminal "producteesTail")]
                       , action = SOME "productee :: productees" }
                     , {atoms = [], action = SOME "[]"} ])
    , ("producteesTail", [ {atoms = [token "Bar", namedNt "productees"], action = SOME "productees"}
                         , {atoms = [], action = SOME "[]"} ])
    , ("productee", [{ atoms = [namedNt "atoms", namedNt "optAction"]
                     , action = SOME "{atoms = atoms, action = optAction}" }])
    , ("atoms", [ {atoms = [namedNt "atom", namedNt "atoms"], action = SOME "atom :: atoms"}
                , {atoms = [], action = SOME "[]"} ])
    , ("atom", [{atoms = [Named ("atom", token "Id")], action = SOME "InputGrammar.NonTerminal (tokenChars atom)"}])
    , ("optAction", [ {atoms = [Named ("action", token "Action")], action = SOME "SOME (tokenChars action)"}
                    , {atoms = [], action = SOME "NONE"} ]) ]

val _ = print (Parsers.parserCode { parserName = "NipoParser"
                                  , tokenType = "NipoTokens.t"
                                  , tokenCtors = [ "Parser", "Lexer", "Id", "Where", "Rules"
                                                 , "Start", "Eq", "Bar", "Action", "Semi" ]
                                  , support = "open NipoTokens"
                                  , rules = grammar
                                  , startRule = "parser" })

