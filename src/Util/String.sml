structure String = struct
    open String

    fun capitalize s =
        case size s
        of 0 => s
         | _ => Substring.concat [ Substring.full (str (Char.toUpper (sub (s, 0))))
                                 , Substring.extract (s, 1, NONE) ]
end

