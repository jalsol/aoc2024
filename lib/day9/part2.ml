let build_blocks input =
    let rec aux rems id blank =
        match rems with
        | [] -> []
        | head :: tail ->
            if blank = false then
                let rest = aux tail id true in
                if head <> 0 then
                    (id, head) :: rest
                else
                    rest
            else
                let rest = aux tail (id + 1) false in
                if head <> 0 then
                    (-1, head) :: rest
                else
                    rest
    in
    aux input 0 false

let rec replace_at lst i new_val =
    match lst with
    | [] -> failwith "not happening"
    | _ :: tl when i = 0 -> new_val :: tl
    | hd :: tl -> hd :: replace_at tl (i - 1) new_val

let rec remove_at lst i =
    match lst with
    | [] -> failwith "not happening"
    | _ :: tl when i = 0 -> tl
    | hd :: tl -> hd :: remove_at tl (i - 1)

let rec insert_at lst i new_val =
  match lst with
  | [] when i = 0 -> [new_val]
  | [] -> failwith "not happening"
  | _ :: _ when i = 0 -> new_val :: lst
  | hd :: tl -> hd :: insert_at tl (i - 1) new_val

let rec defrag blocks right =
    if right < 0 then
        blocks
    else
        let id, cnt = List.nth blocks right in
        if id = -1 then
            defrag blocks (right - 1)
        else
            let rec look_for_space i =
                if i >= right then
                    None
                else
                    let id', cnt' = List.nth blocks i in
                    if id' <> -1 || cnt' < cnt then
                        look_for_space (i + 1)
                    else
                        Some i
            in
            match look_for_space 0 with
            | None -> defrag blocks (right - 1)
            | Some i ->
                (* let _ = Printf.eprintf "Move id %d to %d\n" id i in *)
                let _, cnt' = List.nth blocks i in
                let blocks = remove_at blocks right in
                let blocks = insert_at blocks right (-1, cnt) in
                let blocks = remove_at blocks i in
                let blocks =
                    if cnt' > cnt then
                        insert_at blocks i (-1, cnt' - cnt)
                    else
                        blocks
                    in
                let blocks = insert_at blocks i (id, cnt) in
                (* let _ = List.iter (fun x -> let id, cnt = x in Printf.eprintf "%d %d\n" id cnt) blocks in *)
                (* let _ = prerr_endline "====================================" in *)
                defrag blocks (right - 1)

let flatten_blocks blocks =
    let rec aux acc rems =
        match rems with
        | [] -> acc
        | head :: tail ->
            let id, cnt = head in
            let add = List.init cnt (fun _ -> id) in
            aux (acc @ add) tail
    in
    aux [] blocks

let part2 input =
    let blocks = build_blocks input in
    let blocks = defrag blocks (List.length blocks - 1) in
    let blocks = flatten_blocks blocks in
    let rec aux acc rems i =
        match rems with
        | [] -> acc
        | head :: tail ->
            if head = -1 then
                aux acc tail (i + 1)
            else
                aux (acc + head * i) tail (i + 1)
    in aux 0 blocks 0

