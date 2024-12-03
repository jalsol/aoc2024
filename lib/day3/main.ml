let solve (filename: string) =
    let input = Core.In_channel.read_all filename in
    Printf.printf "%s/Part 1: %d\n" filename (Part1.part1 input);
    Printf.printf "%s/Part 2: %d\n" filename (Part2.part2 input)


let main () =
    solve "sample.txt";
    solve "input.txt";
