let part1 (a: int list) (b: int list) : int =
    let a' = List.sort compare a in
    let b' = List.sort compare b in
    List.fold_left2 (fun acc x y -> acc + abs (x - y)) 0 a' b'

(* ========================================================================== *)

let rec into_map (acc: (int * int) list) (a: int list) : (int * int) list =
    match a with
    | [] -> acc
    | head :: tail ->
        match acc with
        | [] -> into_map [(head, 1)] tail
        | (acc_head, acc_head_cnt) :: acc_tail ->
            if acc_head = head then
                into_map ((head, acc_head_cnt + 1) :: acc_tail) tail
            else
                into_map ((head, 1) :: acc) tail

let rec calc (acc: int) (a: (int * int) list) (b: (int * int) list) : int =
    match a with
    | [] -> acc
    | (a_head, a_head_cnt) :: a_tail ->
        match b with
        | [] -> acc
        | (b_head, b_head_cnt) :: b_tail ->
            if a_head = b_head then
                calc (acc + a_head * a_head_cnt * b_head_cnt) a_tail b_tail
            else if a_head > b_head then
                calc acc a_tail b
            else
                calc acc a b_tail

let part2 (a: int list) (b: int list) : int =
    let a' = List.sort compare a in
    let b' = List.sort compare b in
    let ma = into_map [] a' in
    let mb = into_map [] b' in
    calc 0 ma mb

(* ========================================================================== *)

let main () =
    let lines = Core.In_channel.read_lines "input.txt" in
    let list1, list2 = List.fold_left (fun (list1, list2) line ->
        Scanf.sscanf line "%d %d" (fun a b -> (a :: list1, b :: list2))
    ) ([], []) lines in
    Printf.printf "Part 1: %d\n" (part1 list1 list2);
    Printf.printf "Part 2: %d\n" (part2 list1 list2)
