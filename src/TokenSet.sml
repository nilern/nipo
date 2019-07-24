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
                                    | Predicate _ => true
                                    | Default => false)

    fun predicateCode tokClasses lookahead =
        valOf (foldl (fn (tc, acc) =>
                          let val tcCond =
                                  case Token.patternCode tc
                                  of Predicate pred => pred lookahead
                                   | Pattern pat => lookahead ^ " = " ^ pat
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
                               of SOME (Pattern acc) => SOME (Pattern (acc ^ " | " ^ pat))
                                | SOME Default => acc
                                | NONE => SOME (Pattern pat))
                           | Default => SOME Default)
                     NONE tokClasses)

    fun patternCode tokClasses =
        if requiresPred tokClasses
        then Predicate (predicateCode tokClasses)
        else patCode tokClasses
end

