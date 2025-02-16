let rec is_safe_dir (dir: Part1.dir) (removed: bool) (a: int list) : bool =
    match a with
    | [] -> true
    | _ :: [] -> true
    | first :: second :: rest ->
        if Part1.is_pair_safe dir first second then
            is_safe_dir dir removed (second :: rest)
        else if removed then
            false
        else
            is_safe_dir dir true (first :: rest)

let is_safe (a: int list) : bool =
    match a with
    | [] -> true
    | _ :: tail ->
        is_safe_dir Inc false a || is_safe_dir Dec false a ||
        is_safe_dir Inc true tail || is_safe_dir Dec true tail

let part2 (a: int list list) : int =
    Part1.count_if is_safe a
