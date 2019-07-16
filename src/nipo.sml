signature NIPO_TOKEN_SET = sig
    include ORD_SET

    val toString: set -> string
end

functor NipoTokenSet(Token: NIPO_TOKEN) :> NIPO_TOKEN_SET where type item = Token.t = struct
    structure Super = BinarySetFn(struct
        open Token
        type ord_key = t
    end)
    open Super 
    fun toString tokens =
        let val contents =
                foldl (fn (token, SOME acc) => SOME (acc ^ ", " ^ Token.toString token)
                        | (token, NONE) => SOME (Token.toString token))
                      NONE tokens
        in case contents
           of SOME s => "{" ^ s ^ "}"
            | NONE => "{}"
        end
end

infixr 3 <|>
infixr 4 <*>

(* TODO: External DSL *)
(* TODO: Emit code instead of composing closures. *)
(* TODO: LL(1) -> PLL(1) *)
functor NipoParsers(Input: NIPO_INPUT) :> sig
    type atom
    type input_grammar = (string * atom list list) list

    val rule: string -> atom
    val token: Input.token -> atom

    val parser: input_grammar -> string -> (Input.stream -> unit)
    val parserCode: input_grammar -> string -> string
end = struct
    structure Token = Input.Token

    structure Lookahead = struct
        type t = Token.t option

        val compare =
            fn (SOME token, SOME token') => Token.compare (token, token')
             | (SOME _, NONE) => GREATER
             | (NONE, SOME _) => LESS
             | (NONE, NONE) => EQUAL
            
        val toString =
            fn SOME token => Token.toString token
             | NONE => "<EOF>"
    end

    structure NullableToken = struct
        datatype t = Token of Lookahead.t
                   | Epsilon

        val compare =
            fn (Token token, Token token') => Lookahead.compare (token, token')
             | (Token _, Epsilon) => GREATER
             | (Epsilon, Token _) => LESS
             | (Epsilon, Epsilon) => EQUAL

        val toString =
            fn Token token => Lookahead.toString token
             | Epsilon => "<epsilon>"
    end

    structure StringMap = BinaryMapFn(type ord_key = string val compare = String.compare)
    structure FirstSet = NipoTokenSet(NullableToken)
    type first_set = FirstSet.set
    structure FollowSet = struct
        structure Super = NipoTokenSet(Lookahead)
        open Super

        val fromFirstSet =
            FirstSet.foldl (fn (NullableToken.Token token, followSet) => add (followSet, token)
                             | (NullableToken.Epsilon, followSet) => followSet)
                           empty
    end
    type follow_set = FollowSet.set
    type lookahead_set = follow_set

    datatype atom
        = Terminal of Input.token option
        | NonTerminal of string

    val rule = NonTerminal
    val token = Terminal o SOME

    type input_grammar = (string * atom list list) list

    type 'laset branch = {lookaheads: 'laset, productees: atom list list}

    fun predictionSet firstSet followSet =
        if FirstSet.member (firstSet, NullableToken.Epsilon)
        then FollowSet.union ( FollowSet.fromFirstSet firstSet
                             , followSet )
        else FollowSet.fromFirstSet firstSet
 
    exception Changed

    fun atomFirstSet fiSets =
        fn Terminal token => FirstSet.singleton (NullableToken.Token token)
         | NonTerminal name => StringMap.lookup (fiSets, name)

    fun producteeFirstSet fiSets =
        fn atom :: atoms =>
            let val firsts = atomFirstSet fiSets atom
            in if FirstSet.member (firsts, NullableToken.Epsilon)
               then FirstSet.union ( FirstSet.delete (firsts, NullableToken.Epsilon)
                                   , producteeFirstSet fiSets atoms )
               else firsts
            end
         | [] => FirstSet.singleton NullableToken.Epsilon

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
                #2 (List.foldr atomIteration (followSet, sets') productee)

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

    fun analyze grammar startName =
        let val internalStartName = "start$" ^ startName
            val grammar =
                List.foldl (fn ((name, productees), grammar) =>
                                let val grammar =
                                        StringMap.insert ( grammar, name
                                                         , List.map (fn productee => 
                                                                      { lookaheads = ()
                                                                      , productees = [productee] })
                                                                    productees )
                                in if name = startName
                                   then StringMap.insert ( grammar, internalStartName
                                                         , [{lookaheads = (), productees = [[NonTerminal name, Terminal NONE]]}] )
                                   else grammar
                                end)
                           StringMap.empty grammar
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
        in (grammar, internalStartName, fiSets)
        end

    type parser = Input.stream -> unit

    fun tokenParser name token input =
        let val token' = Input.pop input
        in if token' = token
           then ()
           else raise Fail ( "expected " ^ Lookahead.toString token
                           ^ ", got " ^ Lookahead.toString token' ^ " in " ^ name )
        end

    fun ntParser parser input = valOf (!parser) input

    fun emptyParser _ = ()

    fun atomParser parsers name =
        fn Terminal token => tokenParser name token
         | NonTerminal name' => ntParser (StringMap.lookup (parsers, name'))

    fun seqParser parsers name =
        fn atom :: atoms =>
            let val parseHead = atomParser parsers name atom
                val parseTail = seqParser parsers name atoms
            in fn input => (parseHead input; parseTail input)
            end
         | [] => emptyParser

    fun altParser fiSets parsers name =
        fn {lookaheads, productee} :: productees =>
            let val prediction = lookaheads
                val prediction' = List.foldl FollowSet.union
                                             FollowSet.empty
                                             (List.map #lookaheads productees)
                do if FollowSet.isEmpty (FollowSet.intersection (prediction, prediction'))
                   then ()
                   else raise Fail ( "Conflict: " ^ FollowSet.toString prediction
                                   ^ " intersects with " ^ FollowSet.toString prediction'
                                   ^ " in " ^ name )
                val ntPrediction = FollowSet.union (prediction, prediction')

                val parse = seqParser parsers name productee
                val parse' = altParser fiSets parsers name productees
            in fn input =>
                   let val token = Input.peek input
                   in  if FollowSet.member (prediction, token)
                       then parse input
                       else if FollowSet.member (prediction', token)
                            then parse' input
                            else raise Fail ( "expected one of " ^ FollowSet.toString ntPrediction
                                            ^ ", got " ^ Lookahead.toString token )
                   end
            end
         | [] => fn _ => raise Fail "unreachable"

    fun namedParser fiSets parsers name branches =
        let val branches = List.map (fn {lookaheads, productees = [productee]} => {lookaheads, productee}
                                      | _ => raise Fail ("ambiguous branch in " ^ name))
                                    branches
        in altParser fiSets parsers name branches
        end

    fun parser grammar startName =
        let val (grammar, internalStartName, fiSets) = analyze grammar startName
            val parsers = StringMap.map (fn _ => ref NONE) grammar
            do StringMap.appi (fn (name, branches) =>
                                   let val parser = namedParser fiSets parsers name branches
                                       (*val _ = print ("FIRST(" ^ name ^ ") = " ^ FirstSet.toString (StringMap.lookup (fiSets, name)) ^ "\n")
                                       val _ = print ("FOLLOW(" ^ name ^ ") = " ^ FollowSet.toString (StringMap.lookup (foSets, name)) ^ "\n")*)
                                   in StringMap.lookup (parsers, name) := SOME parser
                                   end)
                              grammar
        in valOf (!(StringMap.lookup (parsers, internalStartName)))
        end

    val preludeCode =
        "fun match token input =\n" ^
        "    let val token' = Input.pop input\n" ^
        "    in  if token' = token\n" ^
        "        then ()\n" ^
        "        else raise Fail (\"expected \" ^ Token.toString token ^ \", got \" ^ Token.toString token')\n" ^
        "    end"

    val tokenPattern =
        fn SOME token => "SOME #\"" ^ Token.toString token ^ "\""
         | NONE => "NONE"

    fun lookaheadPattern lookaheads =
        String.concatWith " | " (List.map tokenPattern (FollowSet.listItems lookaheads))

    val atomCode =
        fn Terminal token => "match (" ^ tokenPattern token ^ ") input"
         | NonTerminal name => name ^ " input"

    val seqCode =
        fn [] => "()"
         | [atom] => atomCode atom
         | atoms => "( " ^ String.concatWith "\n        ; " (List.map atomCode atoms) ^ " )"

    fun branchCode {lookaheads, productees = [productee]} =
        lookaheadPattern lookaheads ^ " =>\n        " ^ seqCode productee

    (* FIXME: Detect conflicts *)
    fun ntCode name branches =
        "fun " ^ name ^ " input =\n"
            ^ "    case Input.peek input\n"
            ^ "    of " ^ String.concatWith "\n     | " (List.map branchCode branches) ^ "\n"
            ^ "     | lookahead =>\n"
            ^ "        raise Fail (\"unexpected \" ^ Lookahead.toString lookahead ^ \" in " ^ name ^ "\")"

    fun parserCode grammar startName =
        let val (grammar, internalStartName, fiSets) = analyze grammar startName
        in preludeCode
           ^ StringMap.foldli (fn (name, branches, acc) => acc ^ "\n\n" ^ ntCode name branches) "" grammar
        end
end

