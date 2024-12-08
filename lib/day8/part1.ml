let collect_stations (grid : string array) (map : (char, (int * int) list) Hashtbl.t) =
    let n = Array.length grid in
    let m = String.length grid.(0) in
    let rec aux i j =
        if i >= n then
            ()
        else if j >= m then
            aux (i + 1) 0
        else
            let ch = grid.(i).[j] in
            if ch <> '.' then
                let _ = match Hashtbl.find_opt map ch with
                | None -> Hashtbl.add map ch [(i, j)]
                | Some tail -> Hashtbl.replace map ch ((i, j) :: tail)
                in
                aux i (j + 1)
            else
                aux i (j + 1)
    in aux 0 0

let find_antinodes (map : (char, (int * int) list) Hashtbl.t) (mark : int array array) =
    let n = Array.length mark in
    let m = Array.length mark.(0) in
    let set_mark x y =
        if 0 <= x && x < n && 0 <= y && y < m then
            mark.(x).(y) <- 1
        else
            ()
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
            let x1', y1' = (x1 - dx, y1 - dy) in
            let x2', y2' = (x2 + dx, y2 + dy) in
            let _ = set_mark x1' y1' in
            let _ = set_mark x2' y2' in
            aux stations i (j + 1)
    in
    Hashtbl.iter (fun _ stations -> aux stations 0 1) map

let part1 (grid : string array) : int =
    let map = Hashtbl.create 36 in
    let _ = collect_stations grid map in
    let n = Array.length grid in
    let m = String.length grid.(0) in
    let mark = Array.make_matrix n m 0 in
    let _ = find_antinodes map mark in
    Array.fold_left (fun acc row -> acc + Array.fold_left ( + ) 0 row) 0 mark

