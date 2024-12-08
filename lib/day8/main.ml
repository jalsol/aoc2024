let solve (filename: string) =
    let lines = Core.In_channel.read_lines filename in
    let grid = lines |> List.to_seq |> Array.of_seq in
    Printf.printf "%s/Part 1: %d\n" filename (Part1.part1 grid);
    Printf.printf "%s/Part 2: %d\n" filename (Part2.part2 grid)

let main () =
    solve "sample.txt";
    solve "input.txt";
