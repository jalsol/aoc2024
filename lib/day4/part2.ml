(* my part 2 is more elegant after going through hell from part 1 *)
(* perhaps gonna check for better answers on the internet *)

let x_mas_count (grid : string array) : int =
    let n = Array.length grid in
    let m = String.length grid.(0) in
    let check i j =
        if grid.(i).[j] <> 'A' then
            false
        else
            let chars = [grid.(i - 1).[j - 1]]
                      @ [grid.(i - 1).[j + 1]]
                      @ [grid.(i + 1).[j - 1]]
                      @ [grid.(i + 1).[j + 1]] in
            let text = String.init (List.length chars) (List.nth chars) in
            text = "MMSS" || text = "MSMS" || text = "SSMM" || text = "SMSM"
    in
    let rec aux acc i j =
        if not (0 <= i - 1 && i + 1 < n) then
            acc
        else if not (0 <= j - 1 && j + 1 < m) then
            aux acc (i + 1) 1
        else if check i j then
            aux (acc + 1) i (j + 1)
        else
            aux acc i (j + 1)
    in aux 0 1 1

let part2 (grid : string array) : int = x_mas_count grid
