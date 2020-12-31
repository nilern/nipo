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

val pos = Pos.default "BootstrapParser.sml"
fun token cs = {pos, v = Var cs}
fun nonTerminal cs = {pos, v = Var cs}
fun named (name, p) = {pos, v = InNamed (name, p)}
fun seq ps = {pos, v = InSeq ps}
fun alt ps = {pos, v = InAlt ps}
val getPos = {pos, v = InPos}

val grammar =
    [ ("parser", [ {productee = nonTerminal "properParser", action = SOME "InputGrammar.Parser properParser"}
                 , {productee = nonTerminal "lexer", action = SOME "InputGrammar.Lexer lexer"} ])
    , ("properParser", [{ productee = seq [ token "Parser", named ("parserName", token "Id"), token "Where"
                                          , nonTerminal "support", nonTerminal "tokens"
                                          , token "Rules", nonTerminal "rules" ]
                        , action = SOME ( "{parserName = tokenChars parserName, rules = #rules rules, startRule = #startRule rules"
                                        ^ ", support = support, tokenCtors = #ctors tokens, tokenType = #typ tokens}" ) }])
    , ("lexer", [{ productee = seq [ token "Lexer", named ("lexerName", token "Id")
                                     , token "Arrow", named ("tokenType", token "Action"), token "Where"
                                     , nonTerminal "support"
                                     , token "Rules", nonTerminal "rules"
                                     , nonTerminal "whitespaceRule" ]
                 , action = SOME ( "{lexerName = tokenChars lexerName, rules = #rules rules @ [whitespaceRule], startRule = #startRule rules"
                                 ^ ", support = support, tokenType = tokenChars tokenType, whitespaceRule = #1 whitespaceRule}" ) }])
    , ("support", [ {productee = seq [named ("supportHeader", token "Action")], action = SOME "tokenChars supportHeader"}
                  , {productee = seq [], action = SOME "\"\""} ])
    , ("tokens", [{ productee = seq [token "Token", named ("typ", token "Action"), token "Eq", nonTerminal "tokenSpecs", token "Semi"]
                  , action = SOME "{typ = tokenChars typ, ctors = tokenSpecs}" }])
    , ("tokenSpecs", [{productee = seq [nonTerminal "tokenSpec", nonTerminal "optTokenSpecs"], action = SOME "tokenSpec :: optTokenSpecs"}])
    , ("optTokenSpecs", [ {productee = seq [token "Bar", nonTerminal "tokenSpecs"], action = SOME "tokenSpecs"}
                        , {productee = seq [], action = SOME "[]"} ])
    , ("tokenSpec", [{ productee = seq [named ("name", token "Id"), named ("alias", nonTerminal "optAlias")]
                     , action = SOME "(tokenChars name, alias)" }])
    , ("optAlias", [ {productee = seq [named ("alias", token "Lit")], action = SOME "SOME (tokenChars alias)"}
                   , {productee = seq [], action = SOME "NONE"} ])
    , ("rules", [{ productee = seq [ named ("starter", nonTerminal "startRule")
                                   , named ("others", nonTerminal "auxRules") ]
                 , action = SOME "{startRule = #1 starter, rules = starter :: others}" }])
    , ("auxRules", [ { productee = seq [nonTerminal "rule", named ("rules", nonTerminal "auxRules")]
                     , action = SOME "rule :: rules" }
                   , { productee = seq [], action = SOME "[]" } ])
    , ("startRule", [{productee = seq [token "Start", nonTerminal "rule"], action = SOME "rule"}])
    , ("whitespaceRule", [{productee = seq [token "Whitespace", nonTerminal "rule"], action = SOME "rule"}])
    , ("rule", [{ productee = seq [named ("name", token "Id"), token "Eq", nonTerminal "clauses", token "Semi"]
                , action = SOME "(tokenChars name, clauses)" }])
    , ("clauses", [{productee = seq [nonTerminal "clause", nonTerminal "optClauses"], action = SOME "clause :: optClauses"}])
    , ("optClauses", [ {productee = seq [token "Bar", nonTerminal "clauses"], action = SOME "clauses"}
                     , {productee = seq [], action = SOME "[]"} ])
    , ("clause", [{ productee = seq [nonTerminal "productee", nonTerminal "optAction"]
                  , action = SOME "{productee = productee, action = optAction}" }])
    , ("productee", [{productee = seq [getPos, nonTerminal "optSeq"], action = SOME "{pos, v = InputGrammar.InSeq optSeq}"}])
    , ("seq", [{productee = seq [nonTerminal "atom", nonTerminal "optSeq"], action = SOME "atom :: optSeq"}])
    , ("optSeq", [ {productee = nonTerminal "seq", action = SOME "seq"}
                 , {productee = seq [], action = SOME "[]"} ])
    , ("atom", [ {productee = seq [ getPos, named ("name", token "Id")
                                  , named ( "res"
                                          , alt [ { productee = seq [token "Eq", nonTerminal "atom"]
                                                  , action = SOME "{pos, v = InputGrammar.InNamed (tokenChars name, atom)}"}
                                                , { productee = token "QMark"
                                                  , action = SOME "{pos, v = InputGrammar.InOpt {pos, v = InputGrammar.Var (tokenChars name)}}"}
                                                , { productee = token "Star"
                                                  , action = SOME "{pos, v = InputGrammar.InMany {pos, v = InputGrammar.Var (tokenChars name)}}"}
                                                , { productee = token "Plus"
                                                  , action = SOME "{pos, v = InputGrammar.InMany1 {pos, v = InputGrammar.Var (tokenChars name)}}"}
                                                , {productee = seq [], action = SOME "{pos, v = InputGrammar.Var (tokenChars name)}"} ] ) ]
                 , action = SOME "res"}
               , { productee = seq [ getPos, nonTerminal "core"
                                   , named ( "res"
                                           , alt [ { productee = token "QMark"
                                                   , action = SOME "{pos, v = InputGrammar.InOpt {pos, v = core}}" }
                                                 , { productee = token "Star"
                                                   , action = SOME "{pos, v = InputGrammar.InMany {pos, v = core}}" }
                                                 , { productee = token "Plus"
                                                   , action = SOME "{pos, v = InputGrammar.InMany1 {pos, v = core}}" }
                                                 , { productee = seq []
                                                   , action = SOME "{pos, v = core}"} ] ) ]
                 , action = SOME "res" } ])
    , ("core", [ {productee = seq [token "LParen", nonTerminal "clauses", token "RParen"], action = SOME "InputGrammar.InAlt clauses"}
               , {productee = named ("alias", token "Lit"), action = SOME "InputGrammar.Lit (tokenChars alias)"}
               , {productee = named ("cclass", token "Posix"), action = SOME "InputGrammar.Posix (tokenChars cclass)"}
               , {productee = token "Pos", action = SOME "InputGrammar.InPos"} ])
    , ("optAction", [ {productee = named ("action", token "Action"), action = SOME "SOME (tokenChars action)"}
                    , {productee = seq [], action = SOME "NONE"} ]) ]

val _ = print (Parsers.parserCode { parserName = "NipoParser"
                                  , tokenType = "NipoTokens.t"
                                  , tokenCtors = List.map (fn name => (name, NONE))
                                                          [ "Parser", "Lexer", "Id", "Lit", "Posix", "Where", "Token", "Rules"
                                                          , "Start", "Whitespace", "Arrow", "Eq", "Bar", "QMark", "Star", "Plus", "Pos"
                                                          , "Action", "LParen", "RParen", "Semi" ]
                                  , support = "open NipoTokens"
                                  , rules = grammar
                                  , startRule = "parser" })

