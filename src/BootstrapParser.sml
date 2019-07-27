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
                                  , obvNamed "support", obvNamed "tokens"
                                  , token "Rules", obvNamed "rules" ]
                        , action = SOME ( "{parserName = tokenChars parserName, rules = #rules rules, startRule = #startRule rules"
                                        ^ ", support = support, tokenCtors = #ctors tokens, tokenType = #typ tokens}" ) }])
    , ("lexer", [{ atoms = [ token "Lexer", InNamed ("lexerName", token "Id")
                           , token "Arrow", InNamed ("tokenType", token "Action"), token "Where"
                           , token "Rules", obvNamed "rules"
                           , obvNamed "whitespaceRule" ]
                 , action = SOME ( "{lexerName = tokenChars lexerName, rules = #rules rules @ [whitespaceRule], startRule = #startRule rules"
                                 ^ ", tokenType = tokenChars tokenType, whitespaceRule = #1 whitespaceRule}" ) }])
    , ("support", [ {atoms = [InNamed ("supportHeader", token "Action")], action = SOME "tokenChars supportHeader"}
                  , {atoms = [], action = SOME "\"\""} ])
    , ("tokens", [{ atoms = [token "Token", InNamed ("typ", token "Action"), token "Eq", obvNamed "tokenSpecs", token "Semi"]
                  , action = SOME "{typ = tokenChars typ, ctors = tokenSpecs}" }])
    , ("tokenSpecs", [{atoms = [obvNamed "tokenSpec", obvNamed "optTokenSpecs"], action = SOME "tokenSpec :: optTokenSpecs"}])
    , ("optTokenSpecs", [ {atoms = [token "Bar", obvNamed "tokenSpecs"], action = SOME "tokenSpecs"}
                        , {atoms = [], action = SOME "[]"} ])
    , ("tokenSpec", [{ atoms = [InNamed ("name", token "Id"), InNamed ("alias", nonTerminal "optAlias")]
                     , action = SOME "(tokenChars name, alias)" }])
    , ("optAlias", [ {atoms = [InNamed ("alias", token "Lit")], action = SOME "SOME (tokenChars alias)"}
                   , {atoms = [], action = SOME "NONE"} ])
    , ("rules", [{ atoms = [ InNamed ("starter", nonTerminal "startRule")
                           , InNamed ("others", nonTerminal "auxRules") ]
                 , action = SOME "{startRule = #1 starter, rules = starter :: others}" }])
    , ("auxRules", [ { atoms = [obvNamed "rule", InNamed ("rules", nonTerminal "auxRules")]
                     , action = SOME "rule :: rules" }
                   , { atoms = [], action = SOME "[]" } ])
    , ("startRule", [{atoms = [token "Start", obvNamed "rule"], action = SOME "rule"}])
    , ("whitespaceRule", [{atoms = [token "Whitespace", obvNamed "rule"], action = SOME "rule"}])
    , ("rule", [{ atoms = [InNamed ("name", token "Id"), token "Eq", InNamed ("productees", nonTerminal "productees"), token "Semi"]
                , action = SOME "(tokenChars name, productees)" }])
    , ("productees", [ { atoms = [obvNamed "productee", InNamed ("productees", nonTerminal "producteesTail")]
                       , action = SOME "productee :: productees" } ])
    , ("producteesTail", [ {atoms = [token "Bar", obvNamed "productees"], action = SOME "productees"}
                         , {atoms = [], action = SOME "[]"} ])
    , ("productee", [{ atoms = [obvNamed "atoms", obvNamed "optAction"]
                     , action = SOME "{atoms = atoms, action = optAction}" }])
    , ("atoms", [ {atoms = [obvNamed "atom", obvNamed "atoms"], action = SOME "atom :: atoms"}
                , {atoms = [], action = SOME "[]"} ])
    , ("atom", [ {atoms = [InNamed ("atom", token "Id")], action = SOME "InputGrammar.Var (tokenChars atom)"}
               , {atoms = [InNamed ("alias", token "Lit")], action = SOME "InputGrammar.Lit (tokenChars alias)"}
               , {atoms = [InNamed ("cclass", token "Posix")], action = SOME "InputGrammar.Posix (tokenChars cclass)"} ])
    , ("optAction", [ {atoms = [InNamed ("action", token "Action")], action = SOME "SOME (tokenChars action)"}
                    , {atoms = [], action = SOME "NONE"} ]) ]

val _ = print (Parsers.parserCode { parserName = "NipoParser"
                                  , tokenType = "NipoTokens.t"
                                  , tokenCtors = List.map (fn name => (name, NONE))
                                                          [ "Parser", "Lexer", "Id", "Lit", "Posix", "Where", "Token", "Rules"
                                                          , "Start", "Whitespace", "Arrow", "Eq", "Bar", "Action", "Semi" ]
                                  , support = "open NipoTokens"
                                  , rules = grammar
                                  , startRule = "parser" })

