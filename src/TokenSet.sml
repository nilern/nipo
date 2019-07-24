signature TOKEN_SET = sig
    include ORD_SET

    val toString: set -> string
    val patternCode: set -> BranchCond.t
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
                                    | Predicate _ => true)

    fun predicateCode tokClasses lookahead =
        valOf (foldl (fn (tc, acc) =>
                          case Token.patternCode tc
                          of Predicate pred =>
                              let val tcCond = pred lookahead
                              in case acc
                                 of SOME acc => SOME (acc ^ " orelse " ^ tcCond)
                                  | NONE => SOME tcCond
                              end)
                     NONE tokClasses)

    fun patCode tokClasses =
        valOf (foldl (fn (tc, acc) =>
                          case Token.patternCode tc
                          of Pattern pat =>
                              (case acc
                               of SOME acc => SOME (acc ^ " | " ^ pat)
                                | NONE => SOME pat))
                     NONE tokClasses)

    fun patternCode tokClasses =
        if isEmpty tokClasses
        then Default (* HACK *)
        else if requiresPred tokClasses
             then Predicate (predicateCode tokClasses)
             else Pattern (patCode tokClasses)
end

