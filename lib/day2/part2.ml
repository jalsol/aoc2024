let rec gen_drop_one (acc: int list list) (index: int) (a: int list) : int list list =
    if index < -1 then
        acc
    else
        let dropped = List.filteri (fun i _ -> i <> index) a in
        gen_drop_one (dropped :: acc) (index - 1) a

let drop_ones (a: int list) =
    gen_drop_one [] (List.length a - 1) a

let is_safe_with_drop (a: int list) : bool =
    List.exists Part1.is_safe (drop_ones a)

let part2 (a: int list list) : int =
    Part1.count_if is_safe_with_drop a
