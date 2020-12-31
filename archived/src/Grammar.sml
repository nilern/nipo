signature GRAMMAR = sig
    structure Token: LEXEME

    datatype productee
        = Alt of clause list
        | Seq of posductee list
        | Opt of posductee
        | Many of posductee
        | Many1 of posductee
        | Named of string * posductee
        | NonTerminal of string
        | Terminal of Token.t option
        | Pos

    withtype posductee = {pos: Pos.t, v: productee}

    and clause = {productee: {pos: Pos.t, v: productee}, action: string option}

    type grammar = (string * clause list) list
end

functor Grammar(Token: LEXEME) :> GRAMMAR
    where type Token.t = Token.t
= struct
    structure Token = Token

    datatype productee
        = Alt of clause list
        | Seq of posductee list
        | Opt of posductee
        | Many of posductee
        | Many1 of posductee
        | Named of string * posductee
        | NonTerminal of string
        | Terminal of Token.t option
        | Pos

    withtype posductee = {pos: Pos.t, v: productee}

    and clause = {productee: {pos: Pos.t, v: productee}, action: string option}

    type grammar = (string * clause list) list
end

structure LexerGrammar = struct
    structure Super = Grammar(CharClass)
    open Super

    type 'grammar glexer =
        { lexerName: string
        , tokenType: string
        , support: string
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
        | InSeq of posductee list
        | InOpt of posductee
        | InMany of posductee
        | InMany1 of posductee
        | Complement of posductee
        | InNamed of string * posductee
        | Var of string
        | Lit of string
        | Posix of string
        | InPos

    withtype posductee = {pos: Pos.t, v: productee}

    and clause = {productee: {pos: Pos.t, v: productee}, action: string option}

    type grammar = (string * clause list) list

    type lexer = grammar LexerGrammar.glexer

    type parser = grammar ParserGrammar.gparser

    datatype t
        = Lexer of lexer
        | Parser of parser
end

signature ANALYZED_GRAMMAR = sig
    type posductee
    type 'laset branch = {lookaheads: 'laset, productees: {productee: posductee, action: string option} list}
    type 'laset grammar = (string * 'laset branch list) list
end

functor AnalyzedGrammar(Grammar: GRAMMAR) :> ANALYZED_GRAMMAR where type posductee = Grammar.posductee = struct
    type posductee = Grammar.posductee
    type 'laset branch = {lookaheads: 'laset, productees: {productee: posductee, action: string option} list}
    type 'laset grammar = (string * 'laset branch list) list
end

