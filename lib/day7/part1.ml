let test (line : int * int list) (ops : (int -> int -> int) list) : int =
    let result, numbers = line in
    let rec aux acc rems =
        match rems with
        | [] -> acc = result
        | head :: tail ->
            List.exists (fun op -> aux (op acc head) tail) ops
    in
    match numbers with
    | [] -> failwith "how the fuck???"
    | head :: tail ->
        if aux head tail then
            result
        else
            0

let part1 (input : (int * int list) list) : int =
    let ops = [( + ); ( * )] in
    List.fold_left (fun acc line -> acc + test line ops) 0 input
