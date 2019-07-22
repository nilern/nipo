structure NipoLexers :> sig
    val lexerCode: string -> string -> Grammar.grammar -> string -> string
end = struct
    fun extractActions grammar =
        let val actions = ref []
            val actionCount = ref 0
            fun extractPredicteeActions {atoms, action} =
                let val actionIndex = !actionCount
                in actions := action :: !actions
                 ; actionCount := actionIndex + 1
                 ; {atoms, action = Int.toString actionIndex}
                end
            val grammar = List.map (fn (name, predictees) =>
                                        (name, List.map extractPredicteeActions predictees))
                                   grammar
        in {grammar, actions = List.rev (!actions)}
        end

    fun actionTableCode actions =
        "    val actions =\n" ^
        "        Vector.fromList [ " ^ String.concatWith "\n                        , " actions ^ " ]\n"

    fun driverCode startName =
        "    fun next input =\n" ^
        "        Option.map (fn _ =>\n" ^
        "                        let val startPos = Input.pos input\n" ^
        "                            val startMark = Input.checkpoint input\n" ^
        "                            val actionIndex = " ^ startName ^ " input\n" ^
        "                            val endPos = Input.pos input\n" ^
        "                            val endMark = Input.checkpoint input\n" ^
        "                            val _ = Input.reset (input, startMark)\n" ^
        "                            val len = #index endPos - #index startPos\n" ^
        "                            (* Slightly breach abstraction to avoid recomputing `endPos`: *)\n" ^
        "                            val recognizedPrefix = Input.Inner.inputN (Input.inner input, len)\n" ^
        "                            val _ = Input.reset (input, endMark)\n" ^
        "                        in Vector.sub (actions, actionIndex) (startPos, recognizedPrefix, endPos)\n" ^
        "                        end)\n" ^
        "                   (Input.peek input)\n"

    fun lexerCode name tokenType grammar startName =
        let val {grammar, actions} = extractActions grammar
        in  "functor " ^ name ^ "(Input: NIPO_LEXER_INPUT) :> NIPO_LEXER\n" ^
            "    where type Input.stream = Input.stream\n" ^
            "    where type Input.checkpoint = Input.checkpoint\n" ^
            "= struct\n" ^
            "    structure Input = Input\n" ^
            "    structure Token = Input.Token\n\n" ^
            "    type token = " ^ tokenType ^ "\n\n" ^
            NipoParsers.matchCode ^
            NipoParsers.recognizerRulesCode grammar startName ^ "\n\n" ^
            actionTableCode actions ^ "\n" ^
            driverCode startName ^
            "end\n"
        end
end

