signature GRAMMAR = sig
    structure Token: LEXEME

    datatype atom = Terminal of Token.t option
                  | NonTerminal of string
                  | Named of string * atom

    type grammar = (string * {atoms: atom list, action: string option} list) list
end

functor Grammar(Token: LEXEME) :> GRAMMAR
    where type Token.t = Token.t
= struct
    structure Token = Token

    datatype atom = Terminal of Token.t option
                  | NonTerminal of string
                  | Named of string * atom

    type grammar = (string * {atoms: atom list, action: string option} list) list
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
    datatype atom
        = Var of string
        | Lit of string
        | Posix of string
        | Complement of atom
        | InNamed of string * atom

    type grammar = (string * {atoms: atom list, action: string option} list) list

    type lexer = grammar LexerGrammar.glexer

    type parser = grammar ParserGrammar.gparser

    datatype t
        = Lexer of lexer
        | Parser of parser
end

signature ANALYZED_GRAMMAR = sig
    type atom
    type 'laset branch = {lookaheads: 'laset, productees: {atoms: atom list, action: string option} list}
    type 'laset grammar = (string * 'laset branch list) list
end

functor AnalyzedGrammar(Grammar: GRAMMAR) :> ANALYZED_GRAMMAR where type atom = Grammar.atom = struct
    type atom = Grammar.atom
    type 'laset branch = {lookaheads: 'laset, productees: {atoms: atom list, action: string option} list}
    type 'laset grammar = (string * 'laset branch list) list
end

