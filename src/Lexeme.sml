structure BranchCond = struct
    datatype t = Pattern of string
               | Predicate of string -> string
               | Default
end

signature LEXEME = sig
    type t

    val compare: t * t -> order
    val overlap: t * t -> bool
    val patternCode: t -> BranchCond.t
    val matchCode: t -> string
    val toString: t -> string
end

structure Token :> LEXEME where type t = string = struct
    type t = string

    fun toString token = token

    val compare = String.compare

    val overlap = op=

    fun patternCode token = BranchCond.Pattern (token ^ " _")

    fun matchCode token = raise Fail "unimplemented"
end

structure CharClass = struct
    open BranchCond

    datatype posix = Alpha

    datatype t = Singleton of char
               | Posix of posix

    val toString =
        fn Singleton c => "'" ^ Char.toString c ^ "'"
         | Posix Alpha => "[:alpha:]"

    val compare = 
        fn (Singleton c, Singleton c') => Char.compare (c, c')
         | (Singleton _, Posix _) => LESS
         | (Posix _, Singleton _) => GREATER
         | (Posix Alpha, Posix Alpha) => EQUAL

    val overlap =
        fn (Singleton c, Singleton c') => c = c'
         | (Singleton c, Posix Alpha)
         | (Posix Alpha, Singleton c) => Char.isAlpha c
         | (Posix Alpha, Posix Alpha) => true

    val patternCode =
        fn Singleton c => Pattern ("#\"" ^ Char.toString c ^ "\"")
         | Posix Alpha => Predicate (fn lookahead => "Char.isAlpha " ^ lookahead)

    fun matchCode cc =
       case patternCode cc
       of Pattern pat => "match (" ^ pat ^ ") input"
        | Predicate pred => "matchPred " ^ pred "" ^ "input"
end

