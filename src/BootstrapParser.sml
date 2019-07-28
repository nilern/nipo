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
    [ ("parser", [ {productee = [InNamed ("parser", nonTerminal "properParser")], action = SOME "InputGrammar.Parser parser"}
                 , {productee = [obvNamed "lexer"], action = SOME "InputGrammar.Lexer lexer"} ])
    , ("properParser", [{ productee = [ token "Parser", InNamed ("parserName", token "Id"), token "Where"
                                  , obvNamed "support", obvNamed "tokens"
                                  , token "Rules", obvNamed "rules" ]
                        , action = SOME ( "{parserName = tokenChars parserName, rules = #rules rules, startRule = #startRule rules"
                                        ^ ", support = support, tokenCtors = #ctors tokens, tokenType = #typ tokens}" ) }])
    , ("lexer", [{ productee = [ token "Lexer", InNamed ("lexerName", token "Id")
                           , token "Arrow", InNamed ("tokenType", token "Action"), token "Where"
                           , token "Rules", obvNamed "rules"
                           , obvNamed "whitespaceRule" ]
                 , action = SOME ( "{lexerName = tokenChars lexerName, rules = #rules rules @ [whitespaceRule], startRule = #startRule rules"
                                 ^ ", tokenType = tokenChars tokenType, whitespaceRule = #1 whitespaceRule}" ) }])
    , ("support", [ {productee = [InNamed ("supportHeader", token "Action")], action = SOME "tokenChars supportHeader"}
                  , {productee = [], action = SOME "\"\""} ])
    , ("tokens", [{ productee = [token "Token", InNamed ("typ", token "Action"), token "Eq", obvNamed "tokenSpecs", token "Semi"]
                  , action = SOME "{typ = tokenChars typ, ctors = tokenSpecs}" }])
    , ("tokenSpecs", [{productee = [obvNamed "tokenSpec", obvNamed "optTokenSpecs"], action = SOME "tokenSpec :: optTokenSpecs"}])
    , ("optTokenSpecs", [ {productee = [token "Bar", obvNamed "tokenSpecs"], action = SOME "tokenSpecs"}
                        , {productee = [], action = SOME "[]"} ])
    , ("tokenSpec", [{ productee = [InNamed ("name", token "Id"), InNamed ("alias", nonTerminal "optAlias")]
                     , action = SOME "(tokenChars name, alias)" }])
    , ("optAlias", [ {productee = [InNamed ("alias", token "Lit")], action = SOME "SOME (tokenChars alias)"}
                   , {productee = [], action = SOME "NONE"} ])
    , ("rules", [{ productee = [ InNamed ("starter", nonTerminal "startRule")
                           , InNamed ("others", nonTerminal "auxRules") ]
                 , action = SOME "{startRule = #1 starter, rules = starter :: others}" }])
    , ("auxRules", [ { productee = [obvNamed "rule", InNamed ("rules", nonTerminal "auxRules")]
                     , action = SOME "rule :: rules" }
                   , { productee = [], action = SOME "[]" } ])
    , ("startRule", [{productee = [token "Start", obvNamed "rule"], action = SOME "rule"}])
    , ("whitespaceRule", [{productee = [token "Whitespace", obvNamed "rule"], action = SOME "rule"}])
    , ("rule", [{ productee = [InNamed ("name", token "Id"), token "Eq", InNamed ("productees", nonTerminal "productees"), token "Semi"]
                , action = SOME "(tokenChars name, productees)" }])
    , ("productees", [ { productee = [obvNamed "productee", InNamed ("productees", nonTerminal "producteesTail")]
                       , action = SOME "productee :: productees" } ])
    , ("producteesTail", [ {productee = [token "Bar", obvNamed "productees"], action = SOME "productees"}
                         , {productee = [], action = SOME "[]"} ])
    , ("productee", [{ productee = [obvNamed "atoms", obvNamed "optAction"]
                     , action = SOME "{productee = atoms, action = optAction}" }])
    , ("atoms", [ {productee = [obvNamed "atom", obvNamed "atoms"], action = SOME "atom :: atoms"}
                , {productee = [], action = SOME "[]"} ])
    , ("atom", [ {productee = [InNamed ("atom", token "Id")], action = SOME "InputGrammar.Var (tokenChars atom)"}
               , {productee = [InNamed ("alias", token "Lit")], action = SOME "InputGrammar.Lit (tokenChars alias)"}
               , {productee = [InNamed ("cclass", token "Posix")], action = SOME "InputGrammar.Posix (tokenChars cclass)"} ])
    , ("optAction", [ {productee = [InNamed ("action", token "Action")], action = SOME "SOME (tokenChars action)"}
                    , {productee = [], action = SOME "NONE"} ]) ]

val _ = print (Parsers.parserCode { parserName = "NipoParser"
                                  , tokenType = "NipoTokens.t"
                                  , tokenCtors = List.map (fn name => (name, NONE))
                                                          [ "Parser", "Lexer", "Id", "Lit", "Posix", "Where", "Token", "Rules"
                                                          , "Start", "Whitespace", "Arrow", "Eq", "Bar", "Action", "Semi" ]
                                  , support = "open NipoTokens"
                                  , rules = grammar
                                  , startRule = "parser" })

