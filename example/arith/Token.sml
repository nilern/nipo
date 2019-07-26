structure ArithToken = struct
    datatype t
        = Plus of Pos.t
        | Times of Pos.t
        | LParen of Pos.t
        | RParen of Pos.t
        | Num of Pos.t * int * Pos.t
    
    val toString =
        fn Plus _ => "+"
         | Times _ => "*"
         | LParen _ => "("
         | RParen _ => ")"
         | Num (_, n, _) => Int.toString n
end

