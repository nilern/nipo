structure StringMap = BinaryMapFn(type ord_key = string val compare = String.compare)

signature GRAMMAR_ANALYSIS = sig
    structure Grammar: GRAMMAR
    structure Analyzed: ANALYZED_GRAMMAR where type posductee = Grammar.posductee
    structure FirstSet: TOKEN_SET
    structure LookaheadSet: FOLLOW_SET

    type conflict =
        { name: string
        , conflict: LookaheadSet.set Analyzed.branch * LookaheadSet.set Analyzed.branch }
    exception Conflicts of conflict list

    val formatConflicts: conflict list -> string

    val firstSet: FirstSet.set StringMap.map -> Grammar.posductee -> FirstSet.set
    val predictionSet: FirstSet.set -> LookaheadSet.set -> LookaheadSet.set
    val analyze: Grammar.grammar -> string -> string option -> string option
              -> LookaheadSet.set Analyzed.branch list StringMap.map
               * FirstSet.set StringMap.map * LookaheadSet.set StringMap.map
end

functor GrammarAnalysis(Args: sig
    structure Grammar: GRAMMAR
    structure Analyzed: ANALYZED_GRAMMAR where type posductee = Grammar.posductee
    structure Lookahead: LEXEME where type t = Grammar.Token.t option
    structure NullableToken: NULLABLE_LEXEME where type non_nullable = Lookahead.t
    structure FirstSet: TOKEN_SET where type item = NullableToken.t
    structure FollowSet: FOLLOW_SET
        where type item = Lookahead.t
        where type FirstSet.set = FirstSet.set
end) :> GRAMMAR_ANALYSIS
    where type Grammar.productee = Args.Grammar.productee
    where type FirstSet.set = Args.FirstSet.set
    where type FirstSet.item = Args.FirstSet.item
    where type LookaheadSet.set = Args.FollowSet.set
    where type LookaheadSet.item = Args.FollowSet.item
= struct
    open BranchCond
    open Matcher
    structure Grammar = Args.Grammar
    structure Analyzed = Args.Analyzed
    structure Token = Grammar.Token
    datatype productee = datatype Grammar.productee
    structure Lookahead = Args.Lookahead
    structure NullableToken = Args.NullableToken
    structure FirstSet = Args.FirstSet
    type first_set = FirstSet.set
    structure FollowSet = Args.FollowSet
    structure LookaheadSet = FollowSet
    type follow_set = FollowSet.set
    type lookahead_set = follow_set

    type conflict =
        { name: string
        , conflict: LookaheadSet.set Analyzed.branch * LookaheadSet.set Analyzed.branch }
    exception Conflicts of conflict list

    fun formatConflict ( { name
                         , conflict = ( {lookaheads, productees = [productee]}
                                      , {lookaheads = lookaheads', productees = [productee']} ) }
                       : conflict ) =
        "Conflict within " ^ name ^ " between\n" ^
        "    " ^ LookaheadSet.toString lookaheads ^ " in " ^ Pos.toString (#pos (#productee productee)) ^ "\n" ^
        "and " ^ LookaheadSet.toString lookaheads ^ " in " ^ Pos.toString (#pos (#productee productee))

    fun formatConflicts conflicts =
        String.concatWith "\n\n" (List.map formatConflict conflicts)

    fun predictionSet firstSet followSet =
        if FirstSet.member (firstSet, NullableToken.Epsilon)
        then FollowSet.union ( FollowSet.fromFirstSet firstSet
                             , followSet )
        else FollowSet.fromFirstSet firstSet
 
    exception Changed

    fun firstSet fiSets {pos = _, v} =
        case v
        of Alt alts => branchFirstSet fiSets alts
         | Seq seq =>
            let val rec seqFirsts =
                    fn p :: ps =>
                        let val firsts = firstSet fiSets p
                        in if FirstSet.member (firsts, NullableToken.Epsilon)
                           then FirstSet.union ( FirstSet.delete (firsts, NullableToken.Epsilon)
                                               , seqFirsts ps )
                           else firsts
                        end
                     | [] => FirstSet.singleton NullableToken.Epsilon
            in seqFirsts seq
            end
         | Opt productee | Many productee =>
            FirstSet.add (firstSet fiSets productee, NullableToken.Epsilon)
         | Many1 productee => firstSet fiSets productee
         | Named (_, inner) => firstSet fiSets inner
         | Terminal token => FirstSet.singleton (NullableToken.Token token)
         | NonTerminal name =>
            (case StringMap.find (fiSets, name)
             of SOME firsts => firsts
              | NONE => raise Fail ("undefined nonterminal " ^ name))
         | Pos => FirstSet.singleton NullableToken.Epsilon

    and clauseFirstSet fiSets {productee, action = _} =
        firstSet fiSets productee

    and branchFirstSet fiSets productees =
        List.foldl FirstSet.union
                   FirstSet.empty
                   (List.map (clauseFirstSet fiSets) productees)

    fun firstSets grammar =
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

    fun followSets grammar fiSets isStart =
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

            fun producteeIteration followSet (productee as {pos = _, v}, sets') =
                case v
                of Alt alts =>
                    List.foldl (clauseIteration followSet) sets' alts
                 | Seq seq =>
                    #2 (List.foldr (fn (productee, (followSet, sets')) =>
                                        ( predictionSet (firstSet fiSets productee) followSet
                                        , producteeIteration followSet (productee, sets') ))
                                   (followSet, sets') seq)
                 | Opt productee => producteeIteration followSet (productee, sets')
                 | Many inner | Many1 inner =>
                    (* The last `inner` is followed by whatever `inner*`/`inner+` is: *)
                    let val sets' = producteeIteration followSet (inner, sets')
                        (* The previous `inner`:s can also be followed by FIRST(productee): *)
                    in producteeIteration (predictionSet (firstSet fiSets productee) followSet)
                                          (inner, sets')
                    end
                 | Named (_, inner) => producteeIteration followSet (inner, sets')
                 | Terminal _ => sets'
                 | NonTerminal name =>
                    let val prev = StringMap.lookup (sets', name)
                    in StringMap.insert (sets', name, FollowSet.union (prev, followSet))
                    end
                 | Pos => sets'

            and clauseIteration followSet ({productee, action = _}, sets') =
                producteeIteration followSet (productee, sets')

            fun branchIteration sets name ({lookaheads = _, productees}, sets') =
                let val followSet = StringMap.lookup (sets, name)
                in List.foldl (clauseIteration followSet) sets' productees
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

    fun analyze grammar startRule whitespaceRule internalStartName =
        let val grammar =
                List.foldl (fn ((name, productees), grammar) =>
                                StringMap.insert ( grammar, name
                                                 , List.map (fn productee => 
                                                              { lookaheads = ()
                                                              , productees = [productee] })
                                                            productees ))
                           StringMap.empty grammar
            val startRulePos = #pos (#productee (hd (#productees (hd (StringMap.lookup (grammar, startRule))))))
            val grammar =
                case internalStartName
                of SOME internalStartName =>
                    StringMap.insert ( grammar, internalStartName
                                     , [{ lookaheads = ()
                                        , productees = [{ productee = { pos = startRulePos
                                                                      , v = Seq [ { pos = startRulePos
                                                                                  , v = Named (startRule, { pos = startRulePos
                                                                                                          , v = NonTerminal startRule }) }
                                                                                , { pos = startRulePos
                                                                                  , v = Terminal NONE } ] }
                                                        , action = SOME startRule } ]}] )
                 | _ => grammar
            val (grammar, fiSets) = firstSets grammar
            val isStart = case internalStartName
                          of SOME startRule => (fn name => name = startRule)
                           | NONE => (fn _ => false)
            val foSets = followSets grammar fiSets isStart
            val grammar = StringMap.mapi (fn (name, branches) =>
                                              let val followSet = StringMap.lookup (foSets, name)
                                              in List.map (fn {lookaheads, productees} =>
                                                               { lookaheads = predictionSet lookaheads followSet
                                                               , productees })
                                                          branches
                                              end)
                                         grammar
        in (grammar, fiSets, foSets)
        end
end

