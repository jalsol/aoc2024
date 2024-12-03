(* cannot really inherit much from part 1, except for copy-pasting... *)

type t = {
    input : string;
    result : int;
    first : int;
    second : int;
    pause : bool;
}

let init_state (input : string) : t = {
    input = input;
    result = 0;
    first = 0;
    second = 0;
    pause = false;
}

let advance_chars (step : int) (state : t) : t =
    let len = String.length state.input in
    let next_input = String.sub state.input step (len - step) in
    { state with input = next_input }

let append_digit_to_first (digit : int) (state : t) : t =
    { state with first = state.first * 10 + digit }

let append_digit_to_second (digit : int) (state : t) : t =
    { state with second = state.second * 10 + digit }

let accumulate_result (state : t) : t =
    let next_result = state.result + state.first * state.second in
    { state with result = next_result; first = 0; second = 0 }

let set_pause (pause : bool) (state : t) : t =
    { state with pause }

let is_digit (ch : char) : bool =
    match ch with
    | '0'..'9' -> true
    | _ -> false

let rec root_state (state : t) : t =
    let len = String.length state.input in
    if len = 0 then
        state
    else
        if String.starts_with ~prefix:"don't()" state.input then
            state
            |> advance_chars 7
            |> set_pause true
            |> root_state
        else if String.starts_with ~prefix:"do()" state.input then
            state
            |> advance_chars 4
            |> set_pause false
            |> root_state
        else if not state.pause && String.starts_with ~prefix:"mul" state.input then
            mul_state (advance_chars 3 state)
        else
            root_state (advance_chars 1 state)

and mul_state (state : t) : t =
    let len = String.length state.input in
    if len = 0 then
        state
    else
        let next_state = advance_chars 1 state in
        if String.get state.input 0 = '(' then
            first_num_state next_state
        else
            root_state { state with first = 0; second = 0 }

and first_num_state (state : t) : t =
    let len = String.length state.input in
    if len = 0 then
        state
    else
        let ch = String.get state.input 0 in
        if is_digit ch then
            state
            |> advance_chars 1
            |> append_digit_to_first (int_of_char ch - int_of_char '0')
            |> first_num_state
        else if ch = ',' then
            advance_chars 1 state
            |> second_num_state
        else
            root_state { state with first = 0; second = 0 }

and second_num_state (state : t) : t =
    let len = String.length state.input in
    if len = 0 then
        state
    else
        let ch = String.get state.input 0 in
        if is_digit ch then
            state
            |> advance_chars 1
            |> append_digit_to_second (int_of_char ch - int_of_char '0')
            |> second_num_state
        else if ch = ')' then
            state
            |> advance_chars 1
            |> accumulate_result
            |> root_state
        else
            root_state { state with first = 0; second = 0 }

let part2 (input : string) : int =
    let final_state = root_state (init_state input) in
    final_state.result
