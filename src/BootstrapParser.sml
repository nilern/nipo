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
datatype atom = datatype InputGrammar.atom

val token = Var
val nonTerminal = Var
fun obvNamed name = InNamed (name, Var name)

val grammar =
    [ ("parser", [ {atoms = [InNamed ("parser", nonTerminal "properParser")], action = SOME "InputGrammar.Parser parser"}
                 , {atoms = [obvNamed "lexer"], action = SOME "InputGrammar.Lexer lexer"} ])
    , ("properParser", [{ atoms = [ token "Parser", InNamed ("parserName", token "Id"), token "Where"
                                  , token "Rules", obvNamed "rules" ]
                        , action = SOME ( "{parserName = tokenChars parserName, rules = #rules rules, startRule = #startRule rules"
                                        ^ ", support = \"\", tokenCtors = [], tokenType = \"\"}" ) }])
    , ("lexer", [{ atoms = [ token "Lexer", InNamed ("lexerName", token "Id"), token "Where"
                           , token "Rules", obvNamed "rules" ]
                 , action = SOME ( "{lexerName = tokenChars lexerName, rules = #rules rules, startRule = #startRule rules"
                                 ^ ", tokenType = \"\", whitespaceRule = \"\"}" ) }])
    , ("rules", [{ atoms = [ InNamed ("starter", nonTerminal "startRule")
                           , InNamed ("others", nonTerminal "auxRules") ]
                 , action = SOME "{startRule = #1 starter, rules = starter :: others}" }])
    , ("auxRules", [ { atoms = [obvNamed "rule", InNamed ("rules", nonTerminal "auxRules")]
                     , action = SOME "rule :: rules" }
                   , { atoms = [], action = SOME "[]" } ])
    , ("startRule", [{atoms = [token "Start", obvNamed "rule"], action = SOME "rule"}])
    , ("rule", [{ atoms = [InNamed ("name", token "Id"), token "Eq", InNamed ("productees", nonTerminal "productees"), token "Semi"]
                , action = SOME "(tokenChars name, productees)" }])
    , ("productees", [ { atoms = [obvNamed "productee", InNamed ("productees", nonTerminal "producteesTail")]
                       , action = SOME "productee :: productees" }
                     , {atoms = [], action = SOME "[]"} ])
    , ("producteesTail", [ {atoms = [token "Bar", obvNamed "productees"], action = SOME "productees"}
                         , {atoms = [], action = SOME "[]"} ])
    , ("productee", [{ atoms = [obvNamed "atoms", obvNamed "optAction"]
                     , action = SOME "{atoms = atoms, action = optAction}" }])
    , ("atoms", [ {atoms = [obvNamed "atom", obvNamed "atoms"], action = SOME "atom :: atoms"}
                , {atoms = [], action = SOME "[]"} ])
    , ("atom", [ {atoms = [InNamed ("atom", token "Id")], action = SOME "InputGrammar.Var (tokenChars atom)"}
               , {atoms = [InNamed ("alias", token "Lit")], action = SOME "InputGrammar.Lit (tokenChars alias)"} ])
    , ("optAction", [ {atoms = [InNamed ("action", token "Action")], action = SOME "SOME (tokenChars action)"}
                    , {atoms = [], action = SOME "NONE"} ]) ]

val _ = print (Parsers.parserCode { parserName = "NipoParser"
                                  , tokenType = "NipoTokens.t"
                                  , tokenCtors = List.map (fn name => (name, NONE))
                                                          [ "Parser", "Lexer", "Id", "Lit", "Where", "Rules"
                                                          , "Start", "Eq", "Bar", "Action", "Semi" ]
                                  , support = "open NipoTokens"
                                  , rules = grammar
                                  , startRule = "parser" })

