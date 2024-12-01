let part1 (a: int list) (b: int list) : int =
    let a' = List.sort compare a in
    let b' = List.sort compare b in
    List.fold_left2 (fun acc x y -> acc + abs (x - y)) 0 a' b'
