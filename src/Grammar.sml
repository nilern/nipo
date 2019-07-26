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

structure LexerGrammar = Grammar(CharClass)
structure ParserGrammar = Grammar(Token)

structure InputGrammar = struct
    type lexer = { lexerName: string
                 , tokenType: string
                 , rules: LexerGrammar.grammar
                 , startRule: string
                 , whitespaceRule: string }

    type parser = { parserName: string
                  , tokenType: string
                  , tokenCtors: string list
                  , support: string
                  , rules: ParserGrammar.grammar
                  , startRule: string }

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

