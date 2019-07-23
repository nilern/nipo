structure NipoTokens = struct
    datatype token
        = Lexer of Pos.t
        | Where of Pos.t
        | Rules of Pos.t
        | Start of Pos.t
        | Eq of Pos.t
        | Bar of Pos.t
        | LBrace of Pos.t
        | RBrace of Pos.t
        | Semi of Pos.t
        | Id of Pos.t * string * Pos.t

    val toString =
        fn Lexer _ => "lexer"
         | Where _ => "where"
         | Rules _ => "rules"
         | Start _ => "start"
         | Eq _ => "="
         | Bar _ => "|"
         | LBrace _ => "{"
         | RBrace _ => "}"
         | Semi _ => ";"
         | Id (_, name, _) => name
end

