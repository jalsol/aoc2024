let d = [ (0, -1); (-1, 0); (0, 1); (1, 0) ]

let find_start (grid : string array) : (int * int) * int =
    let n = Array.length grid in
    let m = String.length grid.(0) in
    let rec aux i j =
        if i >= n then
            failwith "there is no arrow lmfao"
        else if j >= m then
            aux (i + 1) 0
        else if String.contains "<^>v" grid.(i).[j] then
            let dir = String.index "<^>v" grid.(i).[j] in
            (i, j), dir
        else
            aux i (j + 1)
    in
    aux 0 0

let rec visit (grid : string array) (visited : bool array array) (cur : int * int) (dir : int) =
    let n = Array.length grid in
    let m = String.length grid.(0) in
    let x, y = cur in
    let _ = visited.(x).(y) <- true in
    let dx, dy = List.nth d dir in
    let nx = x + dx in
    let ny = y + dy in
    if not (0 <= nx && nx < n && 0 <= ny && ny < m) then
        ()
    else if grid.(nx).[ny] = '#' then
        let ndir = (dir + 1) mod 4 in
        visit grid visited cur ndir
    else
        visit grid visited (nx, ny) dir

let part1 (grid : string array) : int =
    let start, dir = find_start grid in
    let n = Array.length grid in
    let m = String.length grid.(0) in
    let visited = Array.make_matrix n m false in
    let _ = visit grid visited start dir in
    Array.fold_left (fun acc row ->
        acc + Array.fold_left (fun acc x -> acc + if x then 1 else 0) 0 row
    ) 0 visited
