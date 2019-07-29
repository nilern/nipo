structure List = struct
    open List

    fun flatMap f =
        fn x :: xs => f x @ flatMap f xs
         | [] => []
end
