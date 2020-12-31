structure BranchCond = struct
    datatype t = Pattern of string
               | Predicate of string -> string
               | Default
end

structure Matcher = struct
    datatype t = ByValue of string
               | ByPred of string -> string
               | EOF
end

signature LEXEME = sig
    type t

    val eq: t * t -> bool
    val compare: t * t -> order
    val overlap: t * t -> bool
    val patternCode: t -> BranchCond.t
    val stopPatternCode: BranchCond.t
    val matchCode: t -> Matcher.t option
    val stopMatchCode: Matcher.t option
    val toString: t -> string
end

signature NULLABLE_LEXEME = sig
    type non_nullable
    datatype t = Token of non_nullable
               | Epsilon

    val compare: t * t -> order
    val overlap: t * t -> bool
    val patternCode: t -> BranchCond.t
    val stopPatternCode: BranchCond.t
    val matchCode: t -> Matcher.t option
    val stopMatchCode: Matcher.t option
    val toString: t -> string
end

structure Token :> LEXEME where type t = string = struct
    type t = string

    fun toString token = token

    val eq = op=

    val compare = String.compare

    val overlap = op=

    fun patternCode token = BranchCond.Pattern (token ^ " _")

    val stopPatternCode = BranchCond.Pattern "NONE"

    fun matchCode token =
        SOME (Matcher.ByPred (fn lookahead => "is" ^ token ^ " " ^ lookahead))

    val stopMatchCode = SOME Matcher.EOF
end

structure CharClass = struct
    open BranchCond
    open Matcher

    datatype posix = Alpha | Digit | Space

    val comparePosix =
        fn (Alpha, Alpha) => EQUAL
         | (Alpha, Digit | Space) => LESS
         | (Digit, Alpha) => GREATER
         | (Digit, Digit) => EQUAL
         | (Digit, Space) => LESS
         | (Space, Space) => EQUAL
         | (Space, Alpha | Digit) => GREATER

    val classesOverlap =
        fn (Alpha, Alpha) => true
         | (Alpha, _) => false
         | (Digit, Digit) => true
         | (Digit, _) => false
         | (Space, Space) => true
         | (Space, _) => false

    fun charInClass cc c =
        case cc
        of Alpha => Char.isAlpha c
         | Digit => Char.isDigit c
         | Space => Char.isSpace c

    datatype t = Singleton of char
               | Posix of posix
               | Not of t

    val rec toString =
        fn Singleton c => "'" ^ Char.toString c ^ "'"
         | Posix Alpha => "[:alpha:]"
         | Posix Digit => "[:digit:]"
         | Posix Space => "[:space:]"
         | Not cc => "[^" ^ toString cc ^ "]"

    val eq = op=

    val rec compare = 
        fn (Singleton c, Singleton c') => Char.compare (c, c')
         | (Singleton _, Posix _) => LESS
         | (Posix _, Singleton _) => GREATER
         | (Posix pcc, Posix pcc') => comparePosix (pcc, pcc')
         | (Not cc, Not cc') => compare (cc, cc')
         | (Not _, _) => GREATER
         | (_, Not _) => LESS

    val overlap =
        fn (Singleton c, Singleton c') => c = c'
         | (Singleton c, Posix cc) => charInClass cc c
         | (Singleton c, Not t) =>
            (case t
             of Singleton c' => c <> c'
              |_ => raise Fail "unimplemented")
         | (Posix cc, Singleton c) => charInClass cc c
         | (Posix cc, Posix cc') => classesOverlap (cc, cc')
         | (Posix cc, Not t) => raise Fail "unimplemented"
         | (Not t, Singleton c) =>
            (case t
             of Singleton c' => c <> c'
              |_ => raise Fail "unimplemented")
         | (Not t, Posix cc) => raise Fail "unimplemented"
         | (Not t, Not t') => raise Fail "unimplemented"

    val rec patternCode =
        fn Singleton c => Pattern ("#\"" ^ Char.toString c ^ "\"")
         | Posix Alpha => Predicate (fn lookahead => "Char.isAlpha " ^ lookahead)
         | Posix Digit => Predicate (fn lookahead => "Char.isDigit " ^ lookahead)
         | Posix Space => Predicate (fn lookahead => "Char.isSpace " ^ lookahead)
         | Not cc =>
            (case patternCode cc
             of Pattern pat => Predicate (fn lookahead => lookahead ^ " <> " ^ pat)
              | Predicate pred => Predicate (fn lookahead => "not (" ^ pred lookahead ^ ")")
              | Default => Default)

    val stopPatternCode = Default

    fun matchCode cc =
       case patternCode cc
       of Pattern pat => SOME (ByValue pat)
        | Predicate pred => SOME (ByPred pred)
        | Default => NONE

    val stopMatchCode = NONE
end

functor Lookahead(Token: LEXEME) = struct
    open BranchCond

    type t = Token.t option

    val eq =
        fn (SOME t, SOME t') => Token.eq (t, t')
         | (SOME _, NONE) => false
         | (NONE, SOME _) => false
         | (NONE, NONE) => true

    val compare =
        fn (SOME token, SOME token') => Token.compare (token, token')
         | (SOME _, NONE) => GREATER
         | (NONE, SOME _) => LESS
         | (NONE, NONE) => EQUAL

    val overlap =
        fn (SOME token, SOME token') => Token.overlap (token, token')
         | (SOME _, NONE) => false
         | (NONE, SOME _) => false
         | (NONE, NONE) => true

    val toString =
        fn SOME token => Token.toString token
         | NONE => "<EOF>"

    val stopPatternCode = Token.stopPatternCode

    val patternCode =
        fn SOME token => 
            (case Token.patternCode token
             of Pattern pat => Pattern ("SOME (" ^ pat ^ ")")
              | Predicate pred =>
                 Predicate (fn lookahead =>
                                "isSome " ^ lookahead ^ " andalso " ^ pred ("(valOf " ^ lookahead ^ ")"))
              | Default => Default)
         | NONE => stopPatternCode

    val stopMatchCode = Token.stopMatchCode

    fun matchCode lookahead =
        case lookahead
        of SOME token => Token.matchCode token
         | NONE => stopMatchCode
end

functor NullableToken(Lookahead: LEXEME) = struct
    type non_nullable = Lookahead.t
    datatype t = Token of non_nullable
               | Epsilon

    val eq =
        fn (Token t, Token t') => Lookahead.eq(t, t')
         | (Token _, Epsilon) => false
         | (Epsilon, Token _) => false
         | (Epsilon, Epsilon) => true

    val compare =
        fn (Token token, Token token') => Lookahead.compare (token, token')
         | (Token _, Epsilon) => GREATER
         | (Epsilon, Token _) => LESS
         | (Epsilon, Epsilon) => EQUAL

    val overlap =
        fn (Token token, Token token') => Lookahead.overlap (token, token')
         | (Token _, Epsilon) => true
         | (Epsilon, Token _) => true
         | (Epsilon, Epsilon) => true

    val toString =
        fn Token token => Lookahead.toString token
         | Epsilon => "<epsilon>"

    val patternCode =
        fn Token lookahead => Lookahead.patternCode lookahead
         | Epsilon => BranchCond.Pattern "_"

    val stopPatternCode = Lookahead.stopPatternCode

    val matchCode =
        fn Token lookahead => Lookahead.matchCode lookahead
         | Epsilon => NONE

    val stopMatchCode = Lookahead.stopMatchCode
end

