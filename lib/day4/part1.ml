(* this shit is hell just to keep the balance between purity and performance *)

let horizontal_count (grid : string array) : int =
    let rec aux acc row =
        let len = String.length row in
        if len < 4 then
            acc
        else if String.starts_with ~prefix:"XMAS" row || String.starts_with ~prefix:"SAMX" row then
            aux (acc + 1) (String.sub row 1 (len - 1))
        else
            aux acc (String.sub row 1 (len - 1))
    in
    Array.fold_left (fun acc row -> acc + aux 0 row) 0 grid

let vertical_count (grid : string array) : int =
    let n = Array.length grid in
    let m = String.length grid.(0) in
    let transposed =
        Array.init m (fun j ->
            String.init n (fun i ->
                grid.(i).[j])) in
    horizontal_count transposed

let diagonal_major_count (grid : string array) : int =
    let n = Array.length grid in
    let m = String.length grid.(0) in
    let target1 = "XMAS" in
    let target2 = "SAMX" in
    let rec check' target inc i j =
        if i + 3 >= n || j + 3 >= m then
            false
        else if inc >= 4 then
            true
        else if grid.(i + inc).[j + inc] = target.[inc] then
            check' target (inc + 1) i j
        else
            false
    in
    let check i j = check' target1 0 i j || check' target2 0 i j in
    let rec aux acc i j =
        if i + 3 >= n then
            acc
        else if j + 3 >= m then
            aux acc (i + 1) 0
        else if check i j then
            aux (acc + 1) i (j + 1)
        else
            aux acc i (j + 1)
    in aux 0 0 0

let diagonal_minor_count (grid : string array) : int =
    let reverse_string s =
        let len = String.length s in
        String.init len (fun i -> s.[len - i - 1]) in
    let flipped = Array.map reverse_string grid in
    diagonal_major_count flipped

let part1 (grid : string array) : int =
    let hcnt = horizontal_count grid in
    let vcnt = vertical_count grid in
    let dMcnt = diagonal_major_count grid in
    let dmcnt = diagonal_minor_count grid in
    hcnt + vcnt + dMcnt + dmcnt
