signature TOKEN_SET = sig
    include ORD_SET

    val toString: set -> string
    val patternCode: set -> BranchCond.t
end

signature FOLLOW_SET = sig
    include TOKEN_SET
    structure FirstSet: TOKEN_SET

    val fromFirstSet: FirstSet.set -> set
    val overlap: set * set -> bool
end

functor TokenSet(Token: LEXEME) :> TOKEN_SET where type item = Token.t = struct
    open BranchCond

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

    val requiresPred = exists (fn tc =>
                                   case Token.patternCode tc
                                   of Pattern _ => false
                                    | Predicate _ => true
                                    | Default => false)

    fun predicateCode tokClasses lookahead =
        valOf (foldl (fn (tc, acc) =>
                          case Token.patternCode tc
                          of Predicate pred =>
                              (case acc
                               of SOME acc => SOME (acc ^ " orelse " ^ pred lookahead)
                                | NONE => SOME (pred lookahead))
                           | Pattern pat =>
                              (case acc
                               of SOME acc => SOME (acc ^ " orelse " ^ lookahead ^ " = " ^ pat)
                                | NONE => SOME (lookahead ^ " = " ^ pat))
                           | Default => acc)
                     NONE tokClasses)

    fun patCode tokClasses =
        valOf (foldl (fn (tc, acc) =>
                          case Token.patternCode tc
                          of Pattern pat =>
                              (case acc
                               of SOME (Pattern acc) => SOME (Pattern (acc ^ " | " ^ pat))
                                | SOME (Predicate _) => raise Fail "unreachable"
                                | SOME Default => acc
                                | NONE => SOME (Pattern pat))
                           | Predicate _ => raise Fail "unreachable"
                           | Default => SOME Default)
                     NONE tokClasses)

    fun patternCode tokClasses =
        if requiresPred tokClasses
        then Predicate (predicateCode tokClasses)
        else patCode tokClasses
end

functor FollowSet(Args: sig
    structure Lookahead: LEXEME
    structure NullableToken: NULLABLE_LEXEME where type non_nullable = Lookahead.t
    structure FirstSet: TOKEN_SET where type item = NullableToken.t
end) :> FOLLOW_SET
    where type FirstSet.set = Args.FirstSet.set
    where type item = Args.Lookahead.t
= struct
    structure Lookahead = Args.Lookahead
    structure NullableToken = Args.NullableToken
    structure FirstSet = Args.FirstSet
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

