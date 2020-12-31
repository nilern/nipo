module StringMap = Map.Make (String)

type terminal = string
type nonterminal = string

module First = Set.Make (struct
    type t = terminal option
    let compare tok tok' = match (tok, tok') with
        | (Some tok, Some tok') -> String.compare tok tok'
        | (Some _ , None) -> -1
        | (None, Some _) -> 1
        | (None, None) -> 0
end)

module Lookahead = Set.Make (String)
module Follow = Lookahead

module Symbol = struct
    type t = Terminal of terminal | NonTerminal of nonterminal

    let first firsts = function
        | Terminal tok -> First.singleton (Some tok)
        | NonTerminal name -> StringMap.find name firsts

    let lookahead firsts sym follow = match sym with
        | Terminal tok -> Lookahead.singleton tok
        | NonTerminal name ->
            let first = StringMap.find name firsts in
            First.fold (fun otok lookahead -> match otok with
                | Some tok -> Lookahead.add tok lookahead
                | None -> Follow.union lookahead follow
            ) first Lookahead.empty

    let update_follows firsts follow' follows sym =
        ( lookahead firsts sym follow'
        , match sym with
          | Terminal _ -> follows
          | NonTerminal name ->
              follows |> StringMap.update name (function
                  | Some follow -> Some (Follow.union follow follow')
                  | None -> failwith "unreachable") )
end

module Production = struct
    type t = Symbol.t list * string

    let first firsts (syms, _) = match syms with
        | sym :: _ -> Symbol.first firsts sym
        | [] -> First.singleton None

    let update_follows firsts follow follows (syms, _) =
        snd (List.fold_right (fun sym (follow, follows) ->
            Symbol.update_follows firsts follow follows sym
        ) syms (follow, follows))
end

module Alts = struct
    type t = Production.t list

    let first firsts prods = prods
        |> List.map (Production.first firsts)
        |> List.fold_left First.union First.empty

    let update_follows firsts follow follows alts =
        List.fold_left (Production.update_follows firsts follow) follows alts
end

module Grammar = struct
    type t = Alts.t StringMap.t * string

    let firsts (rules, _) =
        let rec iterate firsts =
            let firsts' = StringMap.map (Alts.first firsts) rules in
            if not (StringMap.equal First.equal firsts' firsts)
            then iterate firsts'
            else firsts' in
        iterate (rules |> StringMap.map (Fun.const First.empty))

    let follows firsts (rules, start) =
        let rec iterate follows =
            let follows' = StringMap.fold (fun name alts follows ->
                Alts.update_follows firsts (StringMap.find name follows) follows alts
            ) rules follows in
            if not (StringMap.equal Follow.equal follows' follows)
            then iterate follows'
            else follows' in
        iterate (rules
            |> StringMap.map (Fun.const Follow.empty)
            |> StringMap.add start (Follow.singleton "EOF"))
end

let example : Grammar.t =
    let open Symbol in
    let rules = 
        [ ("stmt", [ ([NonTerminal "pat"; Terminal "="; NonTerminal "expr"], "def")
                   ; ([NonTerminal "expr"], "expr_stmt") ])
        ; ("pat", [([Terminal "INT"], "const_pat")])
        ; ("expr", [([Terminal "INT"], "const_expr")]) ]
        |> List.fold_left (fun rules (name, lhs) -> StringMap.add name lhs rules)
            StringMap.empty in
    (rules, "stmt")

