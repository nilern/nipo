structure Pos = struct
    type t = { name: string
             , index: int
             , line: int
             , col: int }

    fun default name = {name = name, index = 0, line = 1, col = 1}

    fun advance (c, {name = name, index = index, line = line, col = col}) =
        { name = name
        , index = index + 1
        , line = if c = #"\n" then line + 1 else line
        , col = if c = #"\n" then 1 else col + 1 }
end

signature POSITIONED = sig
    type t

    val pos: t -> Pos.t
end

