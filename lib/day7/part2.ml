let concat (lhs : int) (rhs : int) : int =
    let lhs' = string_of_int lhs in
    let rhs' = string_of_int rhs in
    int_of_string (lhs' ^ rhs')


let part2 (input : (int * int list) list) : int =
    let ops = [( + ); ( * ); concat] in
    List.fold_left (fun acc line -> acc + Part1.test line ops) 0 input
