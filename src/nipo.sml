signature NIPO_INPUT = sig
    type stream
    type token

    structure Token: sig
        type t = token
        val compare: t * t -> order
    end

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
    end

    fun peek (ref cs) =
        Option.map #1 (VectorSlice.getItem cs)

    fun pop (input as ref cs) =
        Option.map (fn (c, cs) => (input := cs; c))
                   (VectorSlice.getItem cs)
end

infixr 3 <|>
infixr 4 <*>

functor NipoParsers(Input: NIPO_INPUT) :> sig
    type rule

    val rule: string -> rule
    val token: Input.token -> rule
    val <|> : rule * rule -> rule
    val <*> : rule * rule -> rule

    val parser: (string * rule) list -> Input.stream -> bool
end = struct
    datatype rule
        = Terminal of Input.token
        | NonTerminal of string
        | Seq of rule * rule
        | Alt of rule * rule

    val rule = NonTerminal
    val token = Terminal
    val op<*> = Seq
    val op<|> = Alt

    structure Grammar = BinaryMapFn(type ord_key = string val compare = String.compare)
    structure TokenSet = BinarySetFn(
        type ord_key = Input.token option
        val compare =
            fn (SOME token, SOME token') => Input.Token.compare (token, token')
             | (SOME _, NONE) => GREATER
             | (NONE, SOME _) => LESS
             | (NONE, NONE) => EQUAL
    )

    exception Changed

    fun firstSet sets =
        fn Terminal token => TokenSet.singleton (SOME token)
         | NonTerminal name => Grammar.lookup (sets, name)
         | Seq (l, r) =>
            let val lfirsts = firstSet sets l
            in if TokenSet.member (lfirsts, NONE)
               then TokenSet.union ( TokenSet.delete (lfirsts, NONE)
                                   , firstSet sets r )
               else lfirsts
            end
         | Alt (l, r) => TokenSet.union (firstSet sets l, firstSet sets r)

    fun firstSets (grammar: rule Grammar.map) =
        let fun changed sets sets' =
                ( Grammar.appi (fn (name, set') =>
                                    let val set = Grammar.lookup (sets, name)
                                    in if TokenSet.isSubset (set', set)
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
        in iterate (Grammar.mapi (fn _ => TokenSet.empty) grammar)
        end

    fun parser grammar =
        let val grammar = List.foldl Grammar.insert' Grammar.empty grammar
            val fiSets = firstSets grammar
        in fn input => raise Fail "unimplemented"
        end
end

