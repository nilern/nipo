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
              | Predicate pred => Predicate (fn lookahead => "not (" ^ pred lookahead ^ ")"))

    val stopPatternCode = Default

    fun matchCode cc =
       case patternCode cc
       of Pattern pat => SOME (ByValue pat)
        | Predicate pred => SOME (ByPred pred)

    val stopMatchCode = NONE
end

