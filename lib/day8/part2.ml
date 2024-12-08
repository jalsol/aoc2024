let find_antinodes (map : (char, (int * int) list) Hashtbl.t) (mark : int array array) =
    let n = Array.length mark in
    let m = Array.length mark.(0) in
    let set_mark x y =
        if 0 <= x && x < n && 0 <= y && y < m then
            let _ = mark.(x).(y) <- 1 in
            true
        else
            false
    in
    let draw_from (cur : int * int) (offset : int * int) =
        let (x, y), (dx, dy) = cur, offset in
        let rec draw x y =
            if set_mark x y then
                let nx, ny = x + dx, y + dy in
                draw nx ny
            else
                ()
        in
        draw x y
    in
    let rec aux stations i j =
        let n = List.length stations in
        if i + 1 >= n then
            ()
        else if j >= n then
            aux stations (i + 1) (i + 2)
        else
            let x1, y1 = List.nth stations i in
            let x2, y2 = List.nth stations j in
            let dx, dy = (x2 - x1, y2 - y1) in
            let _ = draw_from (x1, y1) (-dx, -dy) in
            let _ = draw_from (x2, y2) (dx, dy) in
            aux stations i (j + 1)
    in
    Hashtbl.iter (fun _ stations -> aux stations 0 1) map

let part2 (grid : string array) : int =
    let map = Hashtbl.create 36 in
    let _ = Part1.collect_stations grid map in
    let n = Array.length grid in
    let m = String.length grid.(0) in
    let mark = Array.make_matrix n m 0 in
    let _ = find_antinodes map mark in
    Array.fold_left (fun acc row -> acc + Array.fold_left ( + ) 0 row) 0 mark
