structure StringMap = BinaryMapFn(type ord_key = string val compare = String.compare)

signature PARSERS = sig
    structure Grammar: GRAMMAR

    val matchCode: string
    val matchPredCode: string
    val recognizerRulesCode: Grammar.grammar -> string -> string
    val parserCode: { parserName: string
                    , tokenType: string
                    , tokenCtors: string list
                    , support: string
                    , grammar: Grammar.grammar
                    , startName: string } -> string
end

(* TODO: External DSL *)
(* TODO: LL(1) -> PLL(1) *)
functor NipoParsers(Args: sig
    structure Grammar: GRAMMAR
    structure Lookahead: LEXEME where type t = Grammar.Token.t option
    structure NullableToken: NULLABLE_LEXEME where type non_nullable = Lookahead.t
    structure FirstSet: TOKEN_SET where type item = NullableToken.t
    structure FollowSet: FOLLOW_SET
        where type item = Lookahead.t
        where type FirstSet.set = FirstSet.set
end) :> PARSERS where type Grammar.atom = Args.Grammar.atom= struct
    open BranchCond
    open Matcher
    structure Grammar = Args.Grammar
    structure Token = Grammar.Token
    datatype atom = datatype Grammar.atom
    structure Lookahead = Args.Lookahead
    structure NullableToken = Args.NullableToken
    structure FirstSet = Args.FirstSet
    type first_set = FirstSet.set
    structure FollowSet = Args.FollowSet
    type follow_set = FollowSet.set
    type lookahead_set = follow_set

    type 'laset branch = {lookaheads: 'laset, productees: {atoms: atom list, action: string option} list}

    fun predictionSet firstSet followSet =
        if FirstSet.member (firstSet, NullableToken.Epsilon)
        then FollowSet.union ( FollowSet.fromFirstSet firstSet
                             , followSet )
        else FollowSet.fromFirstSet firstSet
 
    exception Changed

    fun atomFirstSet fiSets =
        fn Terminal token => FirstSet.singleton (NullableToken.Token token)
         | NonTerminal name => StringMap.lookup (fiSets, name)
         | Named (_, inner) => atomFirstSet fiSets inner

    fun producteeFirstSet fiSets {atoms, action = _} =
        let val rec atomsFirstSet =
                fn atom :: atoms =>
                    let val firsts = atomFirstSet fiSets atom
                    in if FirstSet.member (firsts, NullableToken.Epsilon)
                       then FirstSet.union ( FirstSet.delete (firsts, NullableToken.Epsilon)
                                           , atomsFirstSet atoms )
                       else firsts
                    end
                 | [] => FirstSet.singleton NullableToken.Epsilon
        in atomsFirstSet atoms
        end

    fun branchFirstSet fiSets productees =
        List.foldl FirstSet.union
                   FirstSet.empty
                   (List.map (producteeFirstSet fiSets) productees)

    fun firstSets (grammar: unit branch list StringMap.map): first_set branch list StringMap.map * first_set StringMap.map =
        let fun branchIteration sets {lookaheads = _, productees} =
                {lookaheads = branchFirstSet sets productees, productees}

            fun iteration sets =
                StringMap.foldli (fn (name, branches, (grammar, sets')) =>
                                      let val branches' = List.map (branchIteration sets) branches
                                          val firsts = List.foldl FirstSet.union
                                                                  FirstSet.empty
                                                                  (List.map #lookaheads branches')
                                      in ( StringMap.insert (grammar, name, branches')
                                         , StringMap.insert (sets', name, firsts) )
                                      end)
                                 (StringMap.empty, StringMap.empty)
                                 grammar

            fun changed sets sets' =
                ( StringMap.appi (fn (name, set') =>
                                    let val set = StringMap.lookup (sets, name)
                                    in if FirstSet.isSubset (set', set)
                                       then ()
                                       else raise Changed
                                    end)
                               sets'
                ; false )
                handle Changed => true

            fun iterate sets =
                let val (grammar', sets') = iteration sets
                in if changed sets sets'
                   then iterate sets'
                   else (grammar', sets')
                end
        in iterate (StringMap.mapi (fn _ => FirstSet.empty) grammar)
        end

    fun followSets (grammar: first_set branch list StringMap.map) (fiSets: first_set StringMap.map) internalStartName
            : lookahead_set StringMap.map =
        let val isStart = case internalStartName
                          of SOME startName => (fn name => name = startName)
                           | NONE => (fn _ => false)

            fun changed sets sets' =
                ( StringMap.appi (fn (name, set') =>
                                    let val set = StringMap.lookup (sets, name)
                                    in if FollowSet.isSubset (set', set)
                                       then ()
                                       else raise Changed
                                    end)
                               sets'
                ; false )
                handle Changed => true

            fun atomIteration (atom, (followSet, sets')) =
                ( predictionSet (atomFirstSet fiSets atom) followSet
                , case atom
                  of Terminal _ => sets'
                   | NonTerminal name =>
                      let val prev = StringMap.lookup (sets', name)
                      in StringMap.insert (sets', name, FollowSet.union (prev, followSet))
                      end
                   | Named (_, inner) => (* HACK: *) #2 (atomIteration (inner, (followSet, sets'))) )

            fun producteeIteration followSet (productee, sets') =
                #2 (List.foldr atomIteration (followSet, sets') (#atoms productee))

            fun branchIteration sets name ({lookaheads = _, productees}, sets') =
                let val followSet = StringMap.lookup (sets, name)
                in List.foldl (producteeIteration followSet) sets' productees
                end

            fun ntIteration sets (name, branches, sets') =
                List.foldl (branchIteration sets name) sets' branches

            fun iterate sets =
                let val sets' = StringMap.foldli (ntIteration sets) sets grammar
                in if changed sets sets'
                   then iterate sets'
                   else sets'
                end
        in iterate (StringMap.mapi (fn (name, _) =>
                                        if isStart name
                                        then FollowSet.empty
                                        else FollowSet.singleton NONE)
                                   grammar)
        end

    fun analyze grammar startName internalStartName =
        let val grammar =
                List.foldl (fn ((name, productees), grammar) =>
                                StringMap.insert ( grammar, name
                                                 , List.map (fn productee => 
                                                              { lookaheads = ()
                                                              , productees = [productee] })
                                                            productees ))
                           (case internalStartName
                            of SOME internalStartName =>
                                StringMap.insert ( StringMap.empty, internalStartName
                                                 , [{lookaheads = (), productees = [{ atoms = [NonTerminal startName, Terminal NONE]
                                                                                    , action = SOME startName } ]}] )
                             | _ => StringMap.empty)
                           grammar
            val (grammar, fiSets) = firstSets grammar
            val foSets = followSets grammar fiSets internalStartName
        in StringMap.mapi (fn (name, branches) =>
                               let val followSet = StringMap.lookup (foSets, name)
                               in List.map (fn {lookaheads, productees} =>
                                                { lookaheads = predictionSet lookaheads followSet
                                                , productees })
                                           branches
                               end)
                         grammar
        end

    val matchCode =
        "    fun match token input =\n" ^
        "        case Input.pop input\n" ^
        "        of SOME token' =>\n" ^
        "            if token' = token\n" ^
        "            then token'\n" ^
        "            else raise Fail ( \"expecfed \" ^ Input.Token.toString token\n" ^
        "                            ^ \", got \" ^ Input.Token.toString token' )\n" ^
        "         | NONE =>\n" ^
        "            raise Fail ( \"expected \" ^ Input.Token.toString token\n" ^
        "                       ^ \", got \" ^ Input.Token.lookaheadToString NONE )"

    val matchPredCode =
        "    fun matchPred pred input =\n" ^
        "        case Input.pop input\n" ^
        "        of SOME token' =>\n" ^
        "            if pred token'\n" ^
        "            then token'\n" ^
        "            else raise Fail (\"unexpected \" ^ Input.Token.toString token')\n" ^
        "         | NONE =>\n" ^
        "            raise Fail (\"unexpected \" ^ Input.Token.lookaheadToString NONE)"

    val matchEOFCode =
        "    fun matchEOF input =\n" ^
        "        case Input.pop input\n" ^
        "        of NONE => ()\n" ^
        "         | SOME token' =>\n" ^
        "            raise Fail ( \"expected \" ^ Input.Token.lookaheadToString NONE\n" ^
        "                       ^ \", got \" ^ Input.Token.toString token' )"

    fun ctorPredicateDef ctor =
        "val is" ^ ctor ^ " = fn " ^ ctor ^ " _ => true | _ => false"

    fun ctorPredicates tokenCtors =
        "    " ^ String.concatWith "\n    " (List.map ctorPredicateDef tokenCtors)

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

    fun producteeCode {atoms, action} =
        let val stmts = List.concat (List.map atomCode atoms)
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
                                         {lookaheads = FollowSet.patternCode lookaheads, productees})
                                    branches
            val errorBody = 
                "            raise Fail (\"unexpected \" ^ Input.Token.lookaheadToString lookahead ^ \" in " ^ name ^ "\")"
            val (patternBranches, predBranches) = List.partition isPatternBranch branches
            val (predBranches, defaultBranches) = List.partition isPredicateBranch predBranches
            val defaultBranch =
                case defaultBranches
                of [] => errorBody
                 | [{productees = [productee], ...}] => producteeCode productee
                 | _ => raise Fail (name ^ " has multiple default branches")
        in "    and " ^ name ^ " input =\n"
           ^ "        case Input.peek input\n"
           ^ "        of " ^ String.concatWith "\n         | " (List.map branchCode patternBranches) ^ "\n"
           ^ "         | lookahead =>\n"
           ^ predicateBranchesCode predBranches defaultBranch
        end

    fun rulesCode grammar =
        StringMap.foldli (fn (name, branches, acc) => acc ^ "\n\n" ^ ntCode name branches) "" grammar

    fun recognizerRulesCode grammar startName =
        let val grammar = analyze grammar startName NONE
        in rulesCode grammar
        end

    fun parserCode {parserName, tokenType, tokenCtors, support, grammar, startName} =
        let val internalStartName = "start__" ^ startName
            val grammar = analyze grammar startName (SOME internalStartName)
        in "functor " ^ parserName ^ "(Input: NIPO_INPUT where type Token.t = " ^ tokenType ^ ") = struct\n" ^
           "    " ^ support ^ "\n\n" ^
           ctorPredicates tokenCtors ^ "\n\n" ^
           matchPredCode ^ "\n\n" ^
           matchEOFCode ^
           rulesCode grammar ^
           "end"
        end
end

