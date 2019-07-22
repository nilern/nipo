structure NipoLexers :> sig
    val lexerCode: string -> Grammar.grammar -> string -> string
end = struct
    datatype atom = Char of char

    type input_grammar = (string * atom list list) list

    fun driverCode startName =
        "    fun next input =\n" ^
        "        let val startPos = Input.pos input\n" ^
        "            val mark = Input.checkpoint input\n" ^
        "            val actionIndex = " ^ startName ^ " input\n" ^
        "            val endPos = Input.pos input\n" ^
        "            val _ = Input.reset (input, mark)\n" ^
        "            val len = #index endPos - #index startPos\n" ^
        "            val recognizedPrefix = Input.inputN (input, len)\n" ^
        "        in Vector.sub (actions, actionIndex) (startPos, recognizedPrefix, endPos)\n" ^
        "        end\n"

    fun lexerCode name grammar startName =
        "functor " ^ name ^ "(Input: NIPO_LEXER_INPUT) :> NIPO_LEXER\n" ^
        "    where type Input.stream = Input.stream\n" ^
        "    where type Input.checkpoint = Input.checkpoint\n" ^
        "= struct\n" ^
        "    structure Input = Input\n" ^
        "    structure Token = Input.Token\n\n" ^
        NipoParsers.matchCode ^
        NipoParsers.recognizerRulesCode grammar startName ^ "\n\n" ^
        driverCode startName ^
        "end\n"
end

