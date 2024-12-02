type dir = Inc | Dec

let is_pair_safe (dir: dir) (first: int) (second: int) : bool =
    let mul = if dir = Inc then 1 else -1 in
    let diff = mul * (second - first) in
    1 <= diff && diff <= 3

let rec is_safe_dir (dir: dir) (a: int list) : bool =
    match a with
    | [] -> true
    | first :: tail ->
        match tail with
        | [] -> true
        | second :: _ ->
            if is_pair_safe dir first second then
                is_safe_dir dir tail
            else
                false

let is_safe (a: int list) : bool =
    is_safe_dir Inc a || is_safe_dir Dec a

let part1 (a: (int list) list) : int =
    let lambda = (fun acc elem -> if is_safe elem then acc + 1 else acc) in
    List.fold_left lambda 0 a