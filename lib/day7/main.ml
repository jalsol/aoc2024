let split_result (line : string) : int * string =
    let pos = String.index line ':' in
    let result = String.sub line 0 pos |> int_of_string in
    let remaining = String.sub line (pos + 2) (String.length line - pos - 2) in
    result, remaining

let solve (filename: string) =
    let lines = Core.In_channel.read_lines filename in
    let input = List.map (fun line ->
        let result, remaining = split_result line in
        let numbers = List.map int_of_string (String.split_on_char ' ' remaining) in
        result, numbers
    ) lines in
    Printf.printf "%s/Part 1: %d\n" filename (Part1.part1 input);
    Printf.printf "%s/Part 2: %d\n" filename (Part2.part2 input)

let main () =
    solve "sample.txt";
    solve "input.txt";
