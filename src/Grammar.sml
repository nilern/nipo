signature GRAMMAR = sig
    structure Token: LEXEME

    datatype productee
        = Alt of clause list
        | Seq of productee list
        | Named of string * productee
        | NonTerminal of string
        | Terminal of Token.t option

    withtype clause = {productee: productee, action: string option}

    type grammar = (string * clause list) list
end

functor Grammar(Token: LEXEME) :> GRAMMAR
    where type Token.t = Token.t
= struct
    structure Token = Token

    datatype productee
        = Alt of clause list
        | Seq of productee list
        | Named of string * productee
        | NonTerminal of string
        | Terminal of Token.t option

    withtype clause = {productee: productee, action: string option}

    type grammar = (string * clause list) list
end

structure LexerGrammar = struct
    structure Super = Grammar(CharClass)
    open Super

    type 'grammar glexer =
        { lexerName: string
        , tokenType: string
        , rules: 'grammar
        , startRule: string
        , whitespaceRule: string }

    type lexer = grammar glexer
end

structure ParserGrammar = struct
    structure Super = Grammar(Token)
    open Super

    type 'grammar gparser =
        { parserName: string
        , tokenType: string
        , tokenCtors: (string * string option) list
        , support: string
        , rules: 'grammar
        , startRule: string }

    type parser = grammar gparser
end

structure InputGrammar = struct
    datatype productee
        = InAlt of clause list
        | InSeq of productee list
        | Complement of productee
        | InNamed of string * productee
        | Var of string
        | Lit of string
        | Posix of string

    withtype clause = {productee: productee, action: string option}

    type grammar = (string * clause list) list

    type lexer = grammar LexerGrammar.glexer

    type parser = grammar ParserGrammar.gparser

    datatype t
        = Lexer of lexer
        | Parser of parser
end

signature ANALYZED_GRAMMAR = sig
    type productee
    type 'laset branch = {lookaheads: 'laset, productees: {productee: productee, action: string option} list}
    type 'laset grammar = (string * 'laset branch list) list
end

functor AnalyzedGrammar(Grammar: GRAMMAR) :> ANALYZED_GRAMMAR where type productee = Grammar.productee = struct
    type productee = Grammar.productee
    type 'laset branch = {lookaheads: 'laset, productees: {productee: productee, action: string option} list}
    type 'laset grammar = (string * 'laset branch list) list
end

