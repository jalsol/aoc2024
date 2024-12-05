let verify_incorrect_order (rules : bool array array) (order : int array) : int =
    if Part1.verify_order rules order <> 0 then
        0
    else
        let predicate u v = if rules.(u).(v) then -1 else 1 in
        let _ = Array.sort predicate order in
        let n = Array.length order in
        order.(n / 2)

let part2 (rules : bool array array) (orders : int array list) : int =
    List.fold_left (fun acc order -> acc + verify_incorrect_order rules order) 0 orders
