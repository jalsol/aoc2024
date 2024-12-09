let solve filename =
    let input = Core.In_channel.read_all filename in
    let input = String.sub input 0 (String.length input - 1) in
    let nums = input |> String.to_seq |> List.of_seq |> List.map (fun ch -> int_of_char ch - int_of_char '0') in
    (* Printf.printf "%s/Part 1: %d\n" filename (Part1.part1 nums); (1* slow af but works, too lazy to optimize (just came back from flight) *1) *)
    Printf.printf "%s/Part 2: %d\n" filename (Part2.part2 nums)

let main () =
    (* solve "sample.txt"; *)
    solve "input.txt";
