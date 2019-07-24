structure NipoLexers :> sig
    structure Token: LEXEME where type t = CharClass.t
    structure Grammar: GRAMMAR where type Token.t = Token.t

    val lexerCode: { lexerName: string
                   , tokenType: string
                   , rules: Grammar.grammar
                   , startRule: string
                   , whitespaceRule: string } -> string
end = struct
    structure Token = CharClass
    structure Grammar = Grammar(Token)
    structure Parsers = NipoParsers(Grammar)

    fun extractActions grammar startRule =
        let val actions = ref []
            val actionCount = ref 0
            fun extractPredicteeActions name (predictee as {atoms, action}) =
                case action
                of SOME action =>
                    let do if name <> startRule
                           then raise Fail ( "Action code in non-start lexer rule " ^ name
                                           ^ ": " ^ action )
                           else ()
                        val actionIndex = !actionCount
                    in actions := action :: !actions
                     ; actionCount := actionIndex + 1
                     ; {atoms, action = SOME (Int.toString actionIndex)}
                    end
                 | NONE => predictee
            val grammar = List.map (fn (name, predictees) =>
                                        (name, List.map (extractPredicteeActions name) predictees))
                                   grammar
        in {grammar, actions = List.rev (!actions)}
        end

    fun actionTableCode actions =
        "    val actions =\n" ^
        "        Vector.fromList [ " ^ String.concatWith "\n                        , " actions ^ " ]\n"

    fun driverCode startName whitespaceRule =
        "    fun next input =\n" ^
        "        ( " ^ whitespaceRule ^ " input\n" ^
        "        ; Option.map (fn _ =>\n" ^
        "                          let val startPos = Input.pos input\n" ^
        "                              val startMark = Input.checkpoint input\n" ^
        "                              val actionIndex = " ^ startName ^ " input\n" ^
        "                              val endPos = Input.pos input\n" ^
        "                              val endMark = Input.checkpoint input\n" ^
        "                              val _ = Input.reset (input, startMark)\n" ^
        "                              val len = #index endPos - #index startPos\n" ^
        "                              (* Slightly breach abstraction to avoid recomputing `endPos`: *)\n" ^
        "                             val recognizedPrefix = Input.Inner.inputN (Input.toInner input, len)\n" ^
        "                             val _ = Input.reset (input, endMark)\n" ^
        "                          in Vector.sub (actions, actionIndex) (startPos, recognizedPrefix, endPos)\n" ^
        "                          end)\n" ^
        "                     (Input.peek input) )\n"

    fun lexerCode {lexerName, tokenType, rules, startRule, whitespaceRule} =
        let val {grammar, actions} = extractActions rules startRule
        in  "functor " ^ lexerName ^ "(Input: NIPO_LEXER_INPUT) :> NIPO_LEXER\n" ^
            "    where type Input.stream = Input.stream\n" ^
            "    where type Input.checkpoint = Input.checkpoint\n" ^
            "    where type token = " ^ tokenType ^ "\n" ^
            "= struct\n" ^
            "    structure Input = Input\n" ^
            "    structure Token = Input.Token\n\n" ^
            "    type token = " ^ tokenType ^ "\n\n" ^
            Parsers.matchCode ^
            Parsers.recognizerRulesCode grammar startRule ^ "\n\n" ^
            actionTableCode actions ^ "\n" ^
            driverCode startRule whitespaceRule ^
            "end\n"
        end
end

