structure Grammar = struct
    structure Token = struct
        type t = string

        fun toString token = token

        val compare = String.compare
    end

    datatype atom = Terminal of Token.t option
                  | NonTerminal of string

    type grammar = (string * {atoms: atom list, action: string} list) list
end

