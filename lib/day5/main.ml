let rec parse_rules (acc : bool array array) (input : string list) : bool array array * string list =
    match input with
    | [] -> acc, []
    | "" :: tail -> acc, tail
    | rule_str :: tail ->
        let u, v = Scanf.sscanf rule_str "%d|%d" (fun u v -> u, v) in
        let _ = acc.(u).(v) <- true in
        parse_rules acc tail

let rec parse_orders (acc : int array list) (input : string list) : int array list =
    match input with
    | [] -> acc
    | head :: tail ->
        let elem_str = String.split_on_char ',' head in
        let elem_seq = List.to_seq elem_str in
        let elem_arr = Array.of_seq elem_seq in
        let order = Array.map int_of_string elem_arr in
        parse_orders (order :: acc) tail

let parse (input : string list) : bool array array * int array list =
    let rules = Array.make_matrix 100 100 false in
    let rules, remaining = parse_rules rules input in
    let orders = parse_orders [] remaining in
    rules, orders

let solve (filename: string) =
    let input = Core.In_channel.read_lines filename in
    let rules, orders = parse input in
    Printf.printf "%s/Part 1: %d\n" filename (Part1.part1 rules orders);
    Printf.printf "%s/Part 2: %d\n" filename (Part2.part2 rules orders)

let main () =
    solve "sample.txt";
    solve "input.txt";
