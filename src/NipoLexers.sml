structure NipoLexers :> sig
    datatype atom = Char of char

    type input_grammar = (string * atom list list) list 

    val lexerCode: input_grammar -> string -> string
end = struct
    datatype atom = Char of char

    type input_grammar = (string * atom list list) list 

    fun lexerCode grammar startName =
        "TODO"
end
