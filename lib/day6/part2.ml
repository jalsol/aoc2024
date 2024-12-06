(* stupid and slow solution, but it works *)

let replace_char (str : string) (index : int) (ch : char) : string =
    let result = Bytes.of_string str in
    let _ = Bytes.set result index ch in
    Bytes.to_string result

let d = [ (0, -1); (-1, 0); (0, 1); (1, 0) ]

let rec can_exit (grid : string array) (visited : bool array array array) (cur : int * int) (block : int * int) (dir : int) : bool =
    let n = Array.length grid in
    let m = String.length grid.(0) in
    let x, y = cur in
    let _ = visited.(dir).(x).(y) <- true in
    let dx, dy = List.nth d dir in
    let nx = x + dx in
    let ny = y + dy in
    if not (0 <= nx && nx < n && 0 <= ny && ny < m) then
        true
    else if visited.(dir).(nx).(ny) then
        false
    else if grid.(nx).[ny] = '#' || (nx, ny) = block then
        let ndir = (dir + 1) mod 4 in
        can_exit grid visited cur block ndir
    else
        can_exit grid visited (nx, ny) block dir

let attempt (grid : string array) (start : int * int) (dir : int) (block : int * int) : bool =
    let n = Array.length grid in
    let m = String.length grid.(0) in
    let visited = Array.init 4 (fun _ -> Array.make_matrix n m false) in
    not (can_exit grid visited start block dir)

let part2 (grid : string array) : int =
    let n = Array.length grid in
    let m = String.length grid.(0) in
    let start, dir = Part1.find_start grid in
    let rec aux acc i j =
        if i >= n then
            acc
        else if j >= m then
            aux acc (i + 1) 0
        else if not (String.contains "<^>v#" grid.(i).[j]) && attempt grid start dir (i, j) then
            aux (acc + 1) i (j + 1)
        else
            aux acc i (j + 1)
    in
    aux 0 0 0
