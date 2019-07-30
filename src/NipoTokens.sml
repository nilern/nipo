structure NipoTokens = struct
    datatype token
        = Lexer of Pos.t
        | Parser of Pos.t
        | Where of Pos.t
        | Token of Pos.t
        | Rules of Pos.t
        | Start of Pos.t
        | Whitespace of Pos.t
        | Arrow of Pos.t
        | Eq of Pos.t
        | Bar of Pos.t
        | QMark of Pos.t
        | Star of Pos.t
        | Plus of Pos.t
        | LParen of Pos.t
        | RParen of Pos.t
        | LBracket of Pos.t
        | RBracket of Pos.t
        | LBrace of Pos.t
        | RBrace of Pos.t
        | Semi of Pos.t
        | Id of Pos.t * string * Pos.t
        | Lit of Pos.t * string * Pos.t
        | Posix of Pos.t * string * Pos.t
        | Pos of Pos.t
        | Action of Pos.t * string * Pos.t

    type t = token
    type vector = t vector

    fun fromId (span as (s, cs, e)) =
        case cs
        of "lexer" => Lexer s
         | "parser" => Parser s
         | "where" => Where s
         | "token" => Token s
         | "rules" => Rules s
         | "start" => Start s
         | "whitespace" => Whitespace s
         | "pos" => Pos s
         | _ => Id span

    val startPos =
        fn Lexer pos => pos
         | Parser pos => pos
         | Where pos => pos
         | Token pos => pos
         | Rules pos => pos
         | Start pos => pos
         | Whitespace pos => pos
         | Arrow pos => pos
         | Eq pos => pos
         | Bar pos => pos
         | QMark pos => pos
         | Star pos => pos
         | Plus pos => pos
         | LParen pos => pos
         | RParen pos => pos
         | LBracket pos => pos
         | RBracket pos => pos
         | LBrace pos => pos
         | RBrace pos => pos
         | Semi pos => pos
         | Id (pos, _, _) => pos
         | Lit (pos, _, _) => pos
         | Posix (pos, _, _) => pos
         | Pos pos => pos (* Uff! *)
         | Action (pos, _, _) => pos

    val tokenChars =
         fn Id (_, cs, _) => cs
          | Lit (_, cs, _) => cs
          | Posix (_, cs, _) => cs
          | Action (_, cs, _) => cs

    val toString =
        fn Lexer _ => "keyword lexer"
         | Parser _ => "keyword parser"
         | Where _ => "keyword where"
         | Token _ => "keyword token"
         | Rules _ => "keyword rules"
         | Start _ => "keyword start"
         | Whitespace _ => "keyword whitespace"
         | Arrow _ => "operator ->"
         | Eq _ => "operator ="
         | Bar _ => "operator |"
         | QMark _ => "operator ?"
         | Star _ => "operator *"
         | Plus _ => "operator +"
         | LParen _ => "delimiter ("
         | RParen _ => "delimiter )"
         | LBracket _ => "delimiter ["
         | RBracket _ => "delimiter ]"
         | LBrace _ => "delimiter {"
         | RBrace _ => "delimiter }"
         | Semi _ => "terminator ;"
         | Id (_, name, _) => "identifier " ^ name
         | Lit (_, name, _) => "identifier " ^ name
         | Posix (_, name, _) => "[:" ^ name ^ ":]"
         | Pos _ => "keyword pos"
         | Action (_, code, _) => "`" ^ code ^ "`"

    val lookaheadToString =
        fn SOME token => toString token
         | NONE => "EOF"
end

