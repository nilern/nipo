structure NipoLexers :> sig
    val lexerCode: string -> Grammar.grammar -> string -> string
end = struct
    datatype atom = Char of char

    type input_grammar = (string * atom list list) list

    fun driverCode startName =
        "    fun next input =\n" ^
        "        let val startPos = Input.pos input\n" ^
        "            val startMark = Input.checkpoint input\n" ^
        "            val actionIndex = " ^ startName ^ " input\n" ^
        "            val endPos = Input.pos input\n" ^
        "            val endMark = Input.checkpoint input\n" ^
        "            val _ = Input.reset (input, startMark)\n" ^
        "            val len = #index endPos - #index startPos\n" ^
        "            (* Slightly breach abstraction to avoid recomputing `endPos`: *)\n" ^
        "            val recognizedPrefix = Input.Inner.inputN (Input.inner input, len)\n" ^
        "            val _ = Input.reset (input, endMark)\n" ^
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

