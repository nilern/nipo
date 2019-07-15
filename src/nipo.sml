signature NIPO_TOKEN = sig
    type t

    val compare: t * t -> order
    val toString: t -> string
end

signature NIPO_INPUT = sig
    type stream
    eqtype token

    structure Token: NIPO_TOKEN where type t = token

    val peek: stream -> token option
    val pop: stream -> token option
end

structure NipoStringInput :> NIPO_INPUT
    where type stream = char VectorSlice.slice ref
    and type token = char
= struct
    type stream = char VectorSlice.slice ref
    type token = char

    structure Token = struct
        type t = token
        
        val compare = Char.compare

        fun toString c = "'" ^ Char.toString c ^ "'"
    end

    fun peek (ref cs) =
        Option.map #1 (VectorSlice.getItem cs)

    fun pop (input as ref cs) =
        Option.map (fn (c, cs) => (input := cs; c))
                   (VectorSlice.getItem cs)
end

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
functor NipoParsers(Input: NIPO_INPUT) :> sig
    type rule

    val rule: string -> rule
    val token: Input.token -> rule
    val empty: rule
    val <|> : rule * rule -> rule
    val <*> : rule * rule -> rule

    val parser: (string * rule) list -> string -> Input.stream -> unit
end = struct
    structure Token = Input.Token

    datatype rule
        = Terminal of Input.token
        | NonTerminal of string
        | Empty
        | Seq of rule * rule
        | Alt of rule * rule

    val rule = NonTerminal
    val token = Terminal
    val empty = Empty
    val op<*> = Seq
    val op<|> = Alt

    structure NullableToken = struct
        datatype t = Token of Token.t
                   | Epsilon

        val compare =
            fn (Token token, Token token') => Token.compare (token, token')
             | (Token _, Epsilon) => GREATER
             | (Epsilon, Token _) => LESS
             | (Epsilon, Epsilon) => EQUAL

        val toString =
            fn Token token => Token.toString token
             | Epsilon => "<epsilon>"
    end

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

    structure Grammar = BinaryMapFn(type ord_key = string val compare = String.compare)
    structure FirstSet = NipoTokenSet(NullableToken)
    structure FollowSet = struct
        structure Super = NipoTokenSet(Lookahead)
        open Super

        val fromFirstSet =
            FirstSet.foldl (fn (NullableToken.Token token, followSet) => add (followSet, SOME token)
                             | (NullableToken.Epsilon, followSet) => followSet)
                           empty
    end

    fun predictionSet firstSet followSet =
        if FirstSet.member (firstSet, NullableToken.Epsilon)
        then FollowSet.union ( FollowSet.fromFirstSet firstSet
                             , followSet )
        else FollowSet.fromFirstSet firstSet
 
    exception Changed

    fun firstSet fiSets =
        fn Terminal token => FirstSet.singleton (NullableToken.Token token)
         | NonTerminal name => Grammar.lookup (fiSets, name)
         | Empty => FirstSet.singleton NullableToken.Epsilon
         | Seq (l, r) =>
            let val lfirsts = firstSet fiSets l
            in if FirstSet.member (lfirsts, NullableToken.Epsilon)
               then FirstSet.union ( FirstSet.delete (lfirsts, NullableToken.Epsilon)
                                   , firstSet fiSets r )
               else lfirsts
            end
         | Alt (l, r) => FirstSet.union (firstSet fiSets l, firstSet fiSets r)

    fun firstSets (grammar: rule Grammar.map) =
        let fun changed sets sets' =
                ( Grammar.appi (fn (name, set') =>
                                    let val set = Grammar.lookup (sets, name)
                                    in if FirstSet.isSubset (set', set)
                                       then ()
                                       else raise Changed
                                    end)
                               sets'
                ; false )
                handle Changed => true

            fun iterate sets =
                let val sets' = Grammar.map (firstSet sets) grammar
                in if changed sets sets'
                   then iterate sets'
                   else sets'
                end
        in iterate (Grammar.mapi (fn _ => FirstSet.empty) grammar)
        end

    fun followSets grammar startName fiSets =
        let fun changed sets sets' =
                ( Grammar.appi (fn (name, set') =>
                                    let val set = Grammar.lookup (sets, name)
                                    in if FollowSet.isSubset (set', set)
                                       then ()
                                       else raise Changed
                                    end)
                               sets'
                ; false )
                handle Changed => true

            fun ruleIteration (name, rule, sets) =
                let fun update followSet rule sets' =
                        case rule
                        of Terminal _ => sets'
                         | NonTerminal name' =>
                            let val prev = Grammar.lookup (sets, name')
                            in Grammar.insert (sets', name', FollowSet.union (prev, followSet))
                            end
                         | Empty => sets'
                         | Seq (l, r) =>
                            let val sets' = update followSet r sets'
                                val rFirsts = firstSet fiSets r
                                val lFollow = predictionSet rFirsts followSet
                            in update lFollow l sets'
                            end
                         | Alt (l, r) =>
                            let val sets' = update followSet l sets'
                            in update followSet r sets'
                            end
                in update (Grammar.lookup (sets, name)) rule sets
                end

            fun iterate sets =
                let val sets' = Grammar.foldli ruleIteration sets grammar
                in if changed sets sets'
                   then iterate sets'
                   else sets'
                end
        in iterate (Grammar.mapi (fn (name, _) =>
                                      if name = startName
                                      then FollowSet.singleton NONE
                                      else FollowSet.empty)
                                grammar)
        end

    type parser = Input.stream -> unit

    fun tokenParser name token input =
        case Input.pop input
        of SOME token' =>
            if token' = token
            then ()
            else raise Fail ( "expected " ^ Token.toString token
                            ^ ", got " ^ Token.toString token' ^ " in " ^ name )
         | NONE => raise Fail ("EOF reached while expecting " ^ Token.toString token ^ " in " ^ name)

    fun ruleParser fiSets foSets parsers name rule: parser =
        let fun ntParser parser input = valOf (!parser) input

            fun emptyParser _ = ()

            fun seqParser followSet name p q =
                let val p = parser (predictionSet (firstSet fiSets q) followSet) name p
                    val q = parser followSet name q
                in fn input => (p input; q input)
                end

            and altParser followSet name p q =
                let val pfirsts = firstSet fiSets p (* OPTIMIZE *)
                    val qfirsts = firstSet fiSets q (* OPTIMIZE *)
                    val pPrediction = predictionSet pfirsts followSet
                    val qPrediction = predictionSet qfirsts followSet
                    do if FollowSet.isEmpty (FollowSet.intersection (pPrediction, qPrediction))
                       then ()
                       else raise Fail ( "Conflict: " ^ FollowSet.toString pPrediction
                                       ^ " intersects with " ^ FollowSet.toString qPrediction
                                       ^ " in " ^ name )
                    val prediction = FollowSet.union (pPrediction, qPrediction)

                    val p = parser followSet name p
                    val q = parser followSet name q
                in fn input =>
                       let val token = Input.peek input
                       in  if FollowSet.member (pPrediction, token)
                           then p input
                           else if FollowSet.member (qPrediction, token)
                                then q input
                                else raise Fail ( "expected one of " ^ FollowSet.toString prediction
                                                ^ ", got " ^ Lookahead.toString token )
                       end
                end

            and parser followSet name =
                fn Terminal token => tokenParser name token
                 | NonTerminal name => ntParser (Grammar.lookup (parsers, name))
                 | Empty => emptyParser
                 | Seq (p, q) => seqParser followSet name p q
                 | Alt (p, q) => altParser followSet name p q
        in parser (Grammar.lookup (foSets, name)) name rule
        end

    fun parser grammar startName =
        let val grammar = List.foldl Grammar.insert' Grammar.empty grammar
            val fiSets = firstSets grammar
            val foSets = followSets grammar startName fiSets
            val parsers = Grammar.map (fn _ => ref NONE) grammar
            do Grammar.appi (fn (name, rule) =>
                                 let val parser = ruleParser fiSets foSets parsers name rule
                                 in Grammar.lookup (parsers, name) := SOME parser
                                 end)
                            grammar
        in valOf (!(Grammar.lookup (parsers, startName)))
        end
end

