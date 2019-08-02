signature PLL_TRANSFORM = sig
end

functor PLLTransform(Args: sig
    structure Grammar: GRAMMAR
    structure Lookahead: LEXEME where type t = Grammar.Token.t option
    structure LookaheadSet: FOLLOW_SET where type item = Lookahead.t
    structure Analysis: GRAMMAR_ANALYSIS where type LookaheadSet.set = LookaheadSet.set
end) :> PLL_TRANSFORM = struct
    structure Grammar = Args.Grammar
    type posductee = Grammar.posductee
    structure Lookahead = Args.Lookahead
    structure LookaheadSet = Args.LookaheadSet
    structure Analysis = Args.Analysis
    datatype productee = datatype Grammar.productee
    val predictionSet = Analysis.predictionSet

    (*fun pllize fiSets foSets grammar =
        let fun lookaheadSet name =
                predictionSet (StringMap.lookup (fiSets, name))
                              (StringMap.lookup (foSets, name))

            val rec unify: posductee * posductee -> posductee option =
                fn ({pos, v = Named (name, p)}, {v = Named (name', p'), ...}) =>
                    Option.map (fn p => {pos, v = Named (name', {pos, v = Named (name, p)})})
                               (unify (p, p'))
                 | (p as {pos, v = NonTerminal name}, p' as {v = NonTerminal name', ...}) =>
                    if name = name'
                    then SOME p
                    else if LookaheadSet.overlap (lookaheadSet name, lookaheadSet name')
                         then raise Fail "unimplemented"
                         else NONE
                 | (p as {pos, v = Terminal la}, {v = Terminal la', ...}) =>
                    if Lookahead.eq (la, la')
                    then SOME p
                    else NONE
                 | (p as {pos, v = Pos}, {v = Pos, ...}) => SOME p

            fun unifySeqs ({productees, action}, {productees = productees', action = action'}) =
                case (productees, productees')
                of (p :: ps, p' :: ps') =>
                    (case unify (p, p')
                     of SOME p =>
                         let val ps = unifySeqs ({productees = ps, action}, {productees = ps', action = action'})
                             val resName = Util.gensym "result"
                         in [{productee = Seq [p, Named (resName, Alt ps)], action = SOME resName}]
                         end
                      | NONE =>
                         [ {productee = Seq productees, action}
                         , {productee = Seq productees', action = action'} ])
                 | (_ :: _, []) | ([], _ :: _) | ([], []) =>
                    [ {productee = Seq productees, action}
                    , {productee = Seq productees', action = action'} ]

            val rec factorWith =
                fn clause as {productee = Seq productees, action} =>
                    fn clause' as {productee = Seq productees', action = action'} :: clauses =>
                        (case unifySeqs ({productees, action}, {productees = productees', action = action'})
                         of SOME clause => factorWith clause clauses
                          | NONE =>
                             let val (clause, clauses) = factorWith clause clauses
                             in (clause, clause' :: clauses)
                             end)

            val rec factor =
                fn clause :: clauses =>
                    let val (clause', clauses') = factorWith clause clauses
                    in clause' :: factor clauses
                    end
                 | [] => []
        in raise Fail "unimplemented"
        end *)
end

