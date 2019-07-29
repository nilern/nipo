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
datatype in_productee = datatype InputGrammar.productee

val token = Var
val nonTerminal = Var

val grammar =
    [ ("parser", [ {productee = InSeq [InNamed ("parser", nonTerminal "properParser")], action = SOME "InputGrammar.Parser parser"}
                 , {productee = InSeq [Var "lexer"], action = SOME "InputGrammar.Lexer lexer"} ])
    , ("properParser", [{ productee = InSeq [ token "Parser", InNamed ("parserName", token "Id"), token "Where"
                                  , Var "support", Var "tokens"
                                  , token "Rules", Var "rules" ]
                        , action = SOME ( "{parserName = tokenChars parserName, rules = #rules rules, startRule = #startRule rules"
                                        ^ ", support = support, tokenCtors = #ctors tokens, tokenType = #typ tokens}" ) }])
    , ("lexer", [{ productee = InSeq [ token "Lexer", InNamed ("lexerName", token "Id")
                           , token "Arrow", InNamed ("tokenType", token "Action"), token "Where"
                           , token "Rules", Var "rules"
                           , Var "whitespaceRule" ]
                 , action = SOME ( "{lexerName = tokenChars lexerName, rules = #rules rules @ [whitespaceRule], startRule = #startRule rules"
                                 ^ ", tokenType = tokenChars tokenType, whitespaceRule = #1 whitespaceRule}" ) }])
    , ("support", [ {productee = InSeq [InNamed ("supportHeader", token "Action")], action = SOME "tokenChars supportHeader"}
                  , {productee = InSeq [], action = SOME "\"\""} ])
    , ("tokens", [{ productee = InSeq [token "Token", InNamed ("typ", token "Action"), token "Eq", Var "tokenSpecs", token "Semi"]
                  , action = SOME "{typ = tokenChars typ, ctors = tokenSpecs}" }])
    , ("tokenSpecs", [{productee = InSeq [Var "tokenSpec", Var "optTokenSpecs"], action = SOME "tokenSpec :: optTokenSpecs"}])
    , ("optTokenSpecs", [ {productee = InSeq [token "Bar", Var "tokenSpecs"], action = SOME "tokenSpecs"}
                        , {productee = InSeq [], action = SOME "[]"} ])
    , ("tokenSpec", [{ productee = InSeq [InNamed ("name", token "Id"), InNamed ("alias", nonTerminal "optAlias")]
                     , action = SOME "(tokenChars name, alias)" }])
    , ("optAlias", [ {productee = InSeq [InNamed ("alias", token "Lit")], action = SOME "SOME (tokenChars alias)"}
                   , {productee = InSeq [], action = SOME "NONE"} ])
    , ("rules", [{ productee = InSeq [ InNamed ("starter", nonTerminal "startRule")
                           , InNamed ("others", nonTerminal "auxRules") ]
                 , action = SOME "{startRule = #1 starter, rules = starter :: others}" }])
    , ("auxRules", [ { productee = InSeq [Var "rule", InNamed ("rules", nonTerminal "auxRules")]
                     , action = SOME "rule :: rules" }
                   , { productee = InSeq [], action = SOME "[]" } ])
    , ("startRule", [{productee = InSeq [token "Start", Var "rule"], action = SOME "rule"}])
    , ("whitespaceRule", [{productee = InSeq [token "Whitespace", Var "rule"], action = SOME "rule"}])
    , ("rule", [{ productee = InSeq [InNamed ("name", token "Id"), token "Eq", nonTerminal "clauses", token "Semi"]
                , action = SOME "(tokenChars name, clauses)" }])
    , ("clauses", [{productee = InSeq [nonTerminal "clause", nonTerminal "optClauses"], action = SOME "clause :: optClauses"}])
    , ("optClauses", [ {productee = InSeq [token "Bar", nonTerminal "clauses"], action = SOME "clauses"}
                     , {productee = InSeq [], action = SOME "[]"} ])
    , ("clause", [{ productee = InSeq [nonTerminal "productee", nonTerminal "optAction"]
                  , action = SOME "{productee = productee, action = optAction}" }])
    , ("productee", [{productee = nonTerminal "optSeq", action = SOME "InputGrammar.InSeq optSeq"}])
    , ("seq", [{productee = InSeq [nonTerminal "atom", nonTerminal "optSeq"], action = SOME "atom :: optSeq"}])
    , ("optSeq", [ {productee = nonTerminal "seq", action = SOME "seq"}
                 , {productee = InSeq [], action = SOME "[]"} ])
    , ("atom", [ {productee = InNamed ("atom", token "Id"), action = SOME "InputGrammar.Var (tokenChars atom)"}
               , {productee = InNamed ("alias", token "Lit"), action = SOME "InputGrammar.Lit (tokenChars alias)"}
               , {productee = InNamed ("cclass", token "Posix"), action = SOME "InputGrammar.Posix (tokenChars cclass)"} ])
    , ("optAction", [ {productee = InNamed ("action", token "Action"), action = SOME "SOME (tokenChars action)"}
                    , {productee = InSeq [], action = SOME "NONE"} ]) ]

val _ = print (Parsers.parserCode { parserName = "NipoParser"
                                  , tokenType = "NipoTokens.t"
                                  , tokenCtors = List.map (fn name => (name, NONE))
                                                          [ "Parser", "Lexer", "Id", "Lit", "Posix", "Where", "Token", "Rules"
                                                          , "Start", "Whitespace", "Arrow", "Eq", "Bar", "Action", "Semi" ]
                                  , support = "open NipoTokens"
                                  , rules = grammar
                                  , startRule = "parser" })

