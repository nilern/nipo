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
        | Action of Pos.t * string * Pos.t

    type t = token
    type vector = t vector

    val toString =
        fn Lexer _ => "keyword lexer"
         | Where _ => "keyword where"
         | Rules _ => "keyword rules"
         | Start _ => "keyword start"
         | Eq _ => "operator ="
         | Bar _ => "operator |"
         | LBrace _ => "delimiter {"
         | RBrace _ => "delimiter }"
         | Semi _ => "terminator ;"
         | Id (_, name, _) => name
         | Action (_, code, _) => "`" ^ code ^ "`"

    val lookaheadToString =
        fn SOME token => toString token
         | NONE => "EOF"
end

