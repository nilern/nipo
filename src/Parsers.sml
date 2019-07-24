(* TODO: External DSL *)
(* TODO: Emit code instead of composing closures. *)
(* TODO: LL(1) -> PLL(1) *)
functor NipoParsers(Grammar: GRAMMAR) :> sig
    val matchCode: string
    val recognizerRulesCode: Grammar.grammar -> string -> string
    val parserCode: Grammar.grammar -> string -> string
end = struct
    open BranchCond
    structure Token = Grammar.Token
    datatype atom = datatype Grammar.atom

    structure Lookahead = struct
        open BranchCond

        type t = Token.t option

        val compare =
            fn (SOME token, SOME token') => Token.compare (token, token')
             | (SOME _, NONE) => GREATER
             | (NONE, SOME _) => LESS
             | (NONE, NONE) => EQUAL

        val overlap =
            fn (SOME token, SOME token') => Token.overlap (token, token')
             | (SOME _, NONE) => false
             | (NONE, SOME _) => false
             | (NONE, NONE) => true

        val toString =
            fn SOME token => Token.toString token
             | NONE => "<EOF>"

        val patternCode =
            fn SOME token => 
                (case Token.patternCode token
                 of Pattern pat => Pattern ("SOME (" ^ pat ^ ")")
                  | Predicate pred =>
                     Predicate (fn lookahead =>
                                    "isSome " ^ lookahead ^ " andalso " ^ pred lookahead))
             | NONE => Pattern "NONE"

        fun matchCode lookahead =
            case patternCode lookahead
            of Pattern pat => "match (" ^ pat ^ ") input"
             | Predicate pred => "matchPred (fn lookahead => " ^ pred "lookahead" ^ ") input"
    end

    structure NullableToken = struct
        datatype t = Token of Lookahead.t
                   | Epsilon

        val compare =
            fn (Token token, Token token') => Lookahead.compare (token, token')
             | (Token _, Epsilon) => GREATER
             | (Epsilon, Token _) => LESS
             | (Epsilon, Epsilon) => EQUAL

        val overlap =
            fn (Token token, Token token') => Lookahead.overlap (token, token')
             | (Token _, Epsilon) => true
             | (Epsilon, Token _) => true
             | (Epsilon, Epsilon) => true

        val toString =
            fn Token token => Lookahead.toString token
             | Epsilon => "<epsilon>"

        val patternCode =
            fn Token lookahead => Lookahead.patternCode lookahead
             | Epsilon => BranchCond.Pattern "_"

        val matchCode =
            fn Token lookahead => Lookahead.matchCode lookahead
             | Epsilon => "()"
    end

    structure StringMap = BinaryMapFn(type ord_key = string val compare = String.compare)
    structure FirstSet = TokenSet(NullableToken)
    type first_set = FirstSet.set
    structure FollowSet = struct
        structure Super = TokenSet(Lookahead)
        open Super

        val fromFirstSet =
            FirstSet.foldl (fn (NullableToken.Token token, followSet) => add (followSet, token)
                             | (NullableToken.Epsilon, followSet) => followSet)
                           empty

        fun overlap (foSet, foSet') =
            foldl (fn (lookahead, res) =>
                       foldl (fn (lookahead', res) =>
                                  res orelse Lookahead.overlap (lookahead, lookahead'))
                             res foSet')
                  false foSet
    end
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

    fun followSets (grammar: first_set branch list StringMap.map) (fiSets: first_set StringMap.map)
            : lookahead_set StringMap.map =
        let fun changed sets sets' =
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
                      end )

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
        in iterate (StringMap.mapi (fn _ => FollowSet.empty) grammar)
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
                                                                                    , action = SOME "startName" } ]}] )
                             | _ => StringMap.empty)
                           grammar
            val (grammar, fiSets) = firstSets grammar
            val foSets = followSets grammar fiSets
            val grammar = StringMap.mapi (fn (name, branches) =>
                                              let val followSet = StringMap.lookup (foSets, name)
                                              in List.map (fn {lookaheads, productees} =>
                                                               { lookaheads = predictionSet lookaheads followSet
                                                               , productees })
                                                          branches
                                              end)
                                         grammar
        in (grammar, fiSets)
        end

    val matchCode =
        "    fun match token input =\n" ^
        "        let val token' = Input.pop input\n" ^
        "        in  if token' = token\n" ^
        "            then ()\n" ^
        "            else raise Fail ( \"expected \" ^ Token.lookaheadToString token\n" ^
        "                            ^ \" got \" ^ Token.lookaheadToString token' )\n" ^
        "        end"

    val isPatternBranch =
        fn {lookaheads = Pattern _, ...} => true
         | {lookaheads = Predicate _, ...} => false

    val atomCode =
        fn Terminal token => Lookahead.matchCode token
         | NonTerminal name => name ^ " input"

    val seqCode =
        fn [] => "()"
         | [atom] => atomCode atom
         | atoms => "( " ^ String.concatWith "\n            ; " (List.map atomCode atoms) ^ " )"

    fun producteeCode {atoms, action} =
        let val codes = List.map atomCode atoms
            val codes = case action
                        of SOME action => codes @ [action]
                         | NONE => codes
        in case codes
           of [] => "()"
            | [code] => code
            | _ => "( " ^ String.concatWith "\n            ; " codes ^ " )"
        end

    fun branchCode name (ruleIndex, {lookaheads = Pattern pat, productees = [productee]}) =
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
                "            raise Fail (\"unexpected \" ^ Token.lookaheadToString lookahead ^ \" in " ^ name ^ "\")"
            val (patternBranches, predBranches) = List.partition isPatternBranch branches
        in "    and " ^ name ^ " input =\n"
           ^ "        case Input.peek input\n"
           ^ "        of " ^ String.concatWith "\n         | "
                                               (Vector.foldr op:: [] (Vector.mapi (branchCode name) (Vector.fromList patternBranches))) ^ "\n"
           ^ "         | lookahead =>\n"
           ^ predicateBranchesCode predBranches errorBody
        end

    fun rulesCode grammar =
        StringMap.foldli (fn (name, branches, acc) => acc ^ "\n\n" ^ ntCode name branches) "" grammar

    fun recognizerRulesCode grammar startName =
        let val (grammar, _) = analyze grammar startName NONE
        in rulesCode grammar
        end

    fun parserCode grammar startName =
        let val internalStartName = "start$" ^ startName
            val (grammar, fiSets) = analyze grammar startName (SOME internalStartName)
        in matchCode
           ^ rulesCode grammar
        end
end

