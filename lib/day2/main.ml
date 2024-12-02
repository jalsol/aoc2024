let solve (filename: string) =
    let lines = Core.In_channel.read_lines filename in
    let nums =
        lines
        |> List.map (String.split_on_char ' ')
        |> List.map (List.map int_of_string) in
    Printf.printf "%s/Part 1: %d\n" filename (Part1.part1 nums);
    Printf.printf "%s/Part 2: %d\n" filename (Part2.part2 nums)


let main () =
    solve "sample.txt";
    solve "input.txt";
