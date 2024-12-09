let build_blocks nums =
    let rec aux acc rems id free =
        match rems with
        | [] -> acc
        | head :: tail ->
            if free = false then
                let add = List.init head (fun _ -> id) in
                aux (acc @ add) tail id true
            else
                let add = List.init head (fun _ -> -1) in
                aux (acc @ add) tail (id + 1) false
    in
    aux [] nums 0 false
    |> List.to_seq
    |> Array.of_seq

let rec solve acc input left right =
    if left > right then
        acc
    else if input.(left) <> -1 then
        solve (acc + input.(left) * left) input (left + 1) right
    else
        if input.(right) = -1 then
            solve acc input left (right - 1)
        else
            solve (acc + input.(right) * left) input (left + 1) (right - 1)

let part1 input =
    let blocks = build_blocks input in
    solve 0 blocks 0 (Array.length blocks - 1)
