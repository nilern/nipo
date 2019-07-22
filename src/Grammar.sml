structure Grammar = struct
    datatype atom = Terminal of string
                  | NonTerminal of string

    type grammar = (string * atom list list) list
end

