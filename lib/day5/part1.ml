let verify_order (rules : bool array array) (order : int array) : int =
    let n = Array.length order in
    let rec aux i j =
        if i >= n then
            true
        else if j >= n then
            aux (i + 1) (i + 2)
        else
            let u = order.(i) in
            let v = order.(j) in
            if rules.(u).(v) then
                aux i (j + 1)
            else
                false
    in
    if aux 0 1 then
        order.(n / 2)
    else
        0

let part1 (rules : bool array array) (orders : int array list) : int =
    List.fold_left (fun acc order -> acc + verify_order rules order) 0 orders
