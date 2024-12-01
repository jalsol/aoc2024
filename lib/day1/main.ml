let main () =
    let lines = Core.In_channel.read_lines "input.txt" in
    let list1, list2 = List.fold_left (fun (list1, list2) line ->
        Scanf.sscanf line "%d %d" (fun a b -> (a :: list1, b :: list2))
    ) ([], []) lines in
    Printf.printf "Part 1: %d\n" (Part1.part1 list1 list2);
    Printf.printf "Part 2: %d\n" (Part2.part2 list1 list2)
