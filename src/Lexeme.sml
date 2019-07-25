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

    datatype posix = Alpha

    datatype t = Singleton of char
               | Posix of posix
               | Not of t

    val rec toString =
        fn Singleton c => "'" ^ Char.toString c ^ "'"
         | Posix Alpha => "[:alpha:]"
         | Not cc => "[^" ^ toString cc ^ "]"

    val rec compare = 
        fn (Singleton c, Singleton c') => Char.compare (c, c')
         | (Singleton _, Posix _) => LESS
         | (Posix _, Singleton _) => GREATER
         | (Posix Alpha, Posix Alpha) => EQUAL
         | (Not cc, Not cc') => compare (cc, cc')
         | (Not _, _) => GREATER
         | (_, Not _) => LESS

    val overlap =
        fn (Singleton c, Singleton c') => c = c'
         | (Singleton c, Posix Alpha)
         | (Posix Alpha, Singleton c) => Char.isAlpha c
         | (Posix Alpha, Posix Alpha) => true

    val rec patternCode =
        fn Singleton c => Pattern ("#\"" ^ Char.toString c ^ "\"")
         | Posix Alpha => Predicate (fn lookahead => "Char.isAlpha " ^ lookahead)
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
    datatype t = Token of Lookahead.t
               | Epsilon

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

    val stopMatchCode = Token.stopMatchCode
end

