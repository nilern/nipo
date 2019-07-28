signature PARSERS_ARGS = sig
    structure Grammar: GRAMMAR
    structure Lookahead: LEXEME where type t = Grammar.Token.t option
    structure Analysis: GRAMMAR_ANALYSIS
        where type LookaheadSet.item = Lookahead.t
        where type Grammar.productee = Grammar.productee
end

signature PARSERS = sig
    structure Grammar: GRAMMAR
    structure Analyzed: ANALYZED_GRAMMAR where type productee = Grammar.productee
    structure LookaheadSet: TOKEN_SET where type item = Grammar.Token.t option

    val matchCode: string
    val matchPredCode: string
    val recognizerRulesCode: Grammar.grammar -> string -> string
    val rulesCode: LookaheadSet.set Analyzed.branch list StringMap.map -> string
end

functor NipoParsers(Args: PARSERS_ARGS) :> PARSERS
    where type Grammar.productee = Args.Grammar.productee
    where type LookaheadSet.set = Args.Analysis.LookaheadSet.set
= struct
    open BranchCond
    open Matcher
    structure Grammar = Args.Grammar
    datatype productee = datatype Grammar.productee
    structure Analysis = Args.Analysis
    structure Analyzed = Analysis.Analyzed
    structure Lookahead = Args.Lookahead
    structure LookaheadSet = Analysis.LookaheadSet
    
    (* FIXME: Error messages in these match routines give position after token: *)

    val matchCode =
        "    fun match token input =\n" ^
        "        case Input.pop input\n" ^
        "        of SOME token' =>\n" ^
        "            if token' = token\n" ^
        "            then token'\n" ^
        "            else raise Fail ( \"expected \" ^ Input.Token.toString token\n" ^
        "                            ^ \", got \" ^ Input.Token.toString token'\n" ^
        "                            ^ \" at \" ^ Input.Pos.toString (Input.pos input) )\n" ^
        "         | NONE =>\n" ^
        "            raise Fail ( \"expected \" ^ Input.Token.toString token\n" ^
        "                       ^ \", got \" ^ Input.Token.lookaheadToString NONE\n" ^
        "                       ^ \" at \" ^ Input.Pos.toString (Input.pos input) )\n"

    val matchPredCode =
        "    fun matchPred pred input =\n" ^
        "        case Input.pop input\n" ^
        "        of SOME token' =>\n" ^
        "            if pred token'\n" ^
        "            then token'\n" ^
        "            else raise Fail ( \"unexpected \" ^ Input.Token.toString token'\n" ^
        "                            ^ \" at \" ^ Input.Pos.toString (Input.pos input) )\n" ^
        "         | NONE =>\n" ^
        "            raise Fail ( \"unexpected \" ^ Input.Token.lookaheadToString NONE" ^
        "                       ^ \" at \" ^ Input.Pos.toString (Input.pos input) )\n"

    val isPatternBranch =
        fn {lookaheads = Pattern _, ...} => true
         | {lookaheads = Predicate _, ...} => false
         | {lookaheads = Default, ...} => false

    val isPredicateBranch =
        fn {lookaheads = Pattern _, ...} => false
         | {lookaheads = Predicate _, ...} => true
         | {lookaheads = Default, ...} => false

    datatype stmt = Val of string * string
                  | Expr of string

    val stmtToString =
        fn Val (name, expr) => "val " ^ name ^ " = " ^ expr
         | Expr expr => "val _ = " ^ expr

    local
        val atomExpr =
            fn Terminal token => 
                (case Lookahead.matchCode token
                 of SOME (Matcher.ByValue const) => "match (" ^ const ^ ") input"
                  | SOME (Matcher.ByPred pred) => "matchPred (fn lookahead => " ^ pred "lookahead" ^ ") input"
                  | SOME Matcher.EOF => "matchEOF input"
                  | NONE => "()")
             | NonTerminal name => name ^ " input"
             | Named _ => raise Fail "unreachable"
        val rec atomStmts =
            fn atom as Terminal _ | atom as NonTerminal _ => [Expr (atomExpr atom)]
             | Named (name, atom) =>
                let val stmts = atomStmts atom
                in case stmts
                   of Val (name', _) :: _ => Val (name, name') :: stmts
                    | Expr expr :: stmts => Val (name, expr) :: stmts
                    | [] => raise Fail "unreachable"
                end
    in  
        fun atomCode atom = List.map stmtToString (List.rev (atomStmts atom))
    end

    fun producteeCode {productee, action} =
        let val stmts = List.concat (List.map atomCode productee)
            val expr = case action
                       of SOME action => action
                        | NONE => "()"
        in case stmts
           of [] => expr
            | _ =>
               "let " ^ String.concatWith "\n                " stmts ^ "\n" ^
               "            in " ^ expr ^ "\n" ^
               "            end"
        end

    fun branchCode {lookaheads = Pattern pat, productees = [productee]} =
        pat ^ " =>\n            " ^ producteeCode productee

    fun predicateBranchesCode predBranches errorBody =
        case predBranches
        of {lookaheads = Predicate pred, productees = [productee]} :: predBranches =>
            "            if " ^ pred "lookahead" ^ "\n" ^
            "            then " ^ producteeCode productee ^ "\n" ^
            "            else " ^ predicateBranchesCode predBranches errorBody
         | [] => errorBody

    (* FIXME: Detect conflicts *)
    fun ntCode name branches =
        let val branches = List.map (fn {lookaheads, productees} =>
                                         {lookaheads = LookaheadSet.patternCode lookaheads, productees})
                                    branches
            val errorBody = 
                "            raise Fail (\"unexpected \" ^ Input.Token.lookaheadToString lookahead ^ \" in " ^ name ^
                " at \" ^ Input.Pos.toString (Input.pos input))"
            val (patternBranches, predBranches) = List.partition isPatternBranch branches
            val (predBranches, defaultBranches) = List.partition isPredicateBranch predBranches
            val defaultBranch =
                case defaultBranches
                of [] => errorBody
                 | [{productees = [productee], ...}] => producteeCode productee
                 | _ => raise Fail (name ^ " has multiple default branches")
        in "    and " ^ name ^ " input =\n" ^
           (case patternBranches
            of [] =>
                "        let val lookahead = Input.peek input\n" ^
                "        in " ^ predicateBranchesCode predBranches defaultBranch ^ "\n" ^
                "        end"
             | _ => 
                "        case Input.peek input\n"
                ^ "        of " ^ String.concatWith "\n         | " (List.map branchCode patternBranches) ^ "\n"
                ^ "         | lookahead =>\n"
                ^ predicateBranchesCode predBranches defaultBranch)
        end

    fun rulesCode grammar =
        StringMap.foldli (fn (name, branches, acc) => acc ^ "\n\n" ^ ntCode name branches) "" grammar

    fun recognizerRulesCode grammar startRule =
        let val grammar = Analysis.analyze grammar startRule NONE
        in rulesCode grammar
        end
end

functor ProperParsers(Args: PARSERS_ARGS where type Analysis.Analyzed.productee = ParserGrammar.productee) = struct
    datatype in_productee = datatype InputGrammar.productee
    datatype productee = datatype ParserGrammar.productee
    structure Analysis = Args.Analysis
    structure Parsers = NipoParsers(Args)
    open Parsers

    fun addTerminalTranslation ((canon, alias), terminals) =
        if StringMap.inDomain (terminals, canon)
        then raise Fail ("duplicate token " ^ canon)
        else let val terminals = StringMap.insert (terminals, canon, canon)
             in case alias
                of SOME alias =>
                    if StringMap.inDomain (terminals, alias)
                    then raise Fail ("duplicate token alias " ^ alias)
                    else StringMap.insert (StringMap.insert (terminals, canon, canon), alias, canon)
                 | NONE => terminals
             end

    fun terminalTranslation tokenCtors =
        List.foldl addTerminalTranslation StringMap.empty tokenCtors

    fun tParsedName canonName = "tok" ^ canonName

    fun ntParserName resName = "parse" ^ String.capitalize resName

    fun nameToAtom terminals name =
        case StringMap.find (terminals, name)
        of SOME canonName => Named (tParsedName canonName, Terminal (SOME canonName))
         | NONE => Named (name, NonTerminal (ntParserName name))

    fun convertAtoms terminals grammar =
        let val rec convertAtom =
                fn Var name =>
                    if Char.isUpper (String.sub (name, 0))
                    then nameToAtom terminals name
                    else Named (name, NonTerminal (ntParserName name))
                 | Lit name => nameToAtom terminals name
                 | InNamed (name, atom) => Named (name, convertAtom atom)

            fun convertProductee {productee, action} =
                {productee = List.map convertAtom productee, action}
            fun convertNt (name, productees) =
                (ntParserName name, List.map convertProductee productees)
        in List.map convertNt grammar
        end

    fun ctorPredicateDef (ctor, _) =
        "val is" ^ ctor ^ " = fn " ^ ctor ^ " _ => true | _ => false"

    fun ctorPredicates tokenCtors =
        "    " ^ String.concatWith "\n    " (List.map ctorPredicateDef tokenCtors)

    val matchEOFCode =
        "    fun matchEOF input =\n" ^
        "        case Input.pop input\n" ^
        "        of NONE => ()\n" ^
        "         | SOME token' =>\n" ^
        "            raise Fail ( \"expected \" ^ Input.Token.lookaheadToString NONE\n" ^
        "                       ^ \", got \" ^ Input.Token.toString token'" ^
        "                       ^ \" at \" ^ Input.Pos.toString (Input.pos input) )\n"

    fun parserCode ({parserName, tokenType, tokenCtors, support, rules, startRule}: InputGrammar.parser) =
        let val internalStartName = "start__" ^ startRule
            val rules = convertAtoms (terminalTranslation tokenCtors) rules
            val grammar = Analysis.analyze rules (ntParserName startRule) (SOME internalStartName)
        in "functor " ^ parserName ^ "(Input: NIPO_PARSER_INPUT where type Token.t = " ^ tokenType ^ ") = struct\n" ^
           "    " ^ support ^ "\n\n" ^
           ctorPredicates tokenCtors ^ "\n\n" ^
           matchPredCode ^ "\n\n" ^
           matchEOFCode ^
           rulesCode grammar ^
           "end"
        end
end

