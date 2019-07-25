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

