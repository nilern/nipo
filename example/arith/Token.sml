structure ArithToken = struct
    datatype t
        = Plus of Pos.t
        | Times of Pos.t
        | LParen of Pos.t
        | RParen of Pos.t
        | Num of Pos.t * int * Pos.t

    type vector = t vector

    val startPos =
        fn Plus pos => pos
         | Times pos => pos
         | LParen pos => pos
         | RParen pos => pos
         | Num (pos, _, _) => pos
    
    val toString =
        fn Plus _ => "+"
         | Times _ => "*"
         | LParen _ => "("
         | RParen _ => ")"
         | Num (_, n, _) => Int.toString n

    val lookaheadToString =
        fn SOME token => toString token
         | NONE => "EOF"
end

