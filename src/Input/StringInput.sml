structure NipoStringInput :> NIPO_INPUT
    where type stream = char VectorSlice.slice ref
    and type token = char
= struct
    type stream = char VectorSlice.slice ref
    type token = char

    structure Token = struct
        type t = token
        
        val compare = Char.compare

        fun toString c = Char.toString c
    end

    fun peek (ref cs) =
        Option.map #1 (VectorSlice.getItem cs)

    fun pop (input as ref cs) =
        Option.map (fn (c, cs) => (input := cs; c))
                   (VectorSlice.getItem cs)
end

