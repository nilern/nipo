structure CharToken = struct
    type t = char
    type vector = string

    val toString = Char.toString

    val lookaheadToString =
        fn SOME c => "'" ^ toString c ^ "'"
         | NONE => "EOF"
end

