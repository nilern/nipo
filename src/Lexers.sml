structure NipoLexers :> sig
    val lexerCode: Grammar.grammar -> string -> string
end = struct
    datatype atom = Char of char

    type input_grammar = (string * atom list list) list

    fun driverCode startName =
        "fun next input =\n" ^
        "    let val mark = Input.checkpoint input\n" ^
        "        val (actionIndex, len) = " ^ startName ^ " input\n" ^
        "        val _ = Input.reset (input, mark)\n" ^
        "        val recognizedPrefix = Input.take (input, len)\n" ^
        "    in Vector.sub (actions, actionIndex) recognizedPrefix\n" ^
        "    end\n"

    fun lexerCode grammar startName =
        NipoParsers.matchCode
        ^ NipoParsers.recognizerRulesCode grammar startName ^ "\n\n"
        ^ driverCode startName
end

