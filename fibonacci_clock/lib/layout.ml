open Core

type t = { color : int; value : int; index : int }
type time = { hour : int; minute : int; add_time : int }

let pl f lst =
  let rec print_elements = function
    | [] -> ()
    | h :: t ->
        Stdlib.print_string "(";
        f (fst h);
        Stdlib.print_string ", ";
        f (snd h);
        Stdlib.print_string "); ";

        (* f h; *)
        print_elements t
  in
  Stdlib.print_string "[";
  print_elements lst;
  Stdlib.print_string "]";
  Stdlib.print_newline ()

let pint = pl Stdlib.print_int

let get_layout time sequence =
  let get_rando_seq target color_value terms =
    if target = 0 then terms
    else
      let rec inner target result term_list =
        match term_list with
        | [] -> failwith "target cannot be reached with these numbers"
        | _ ->
            let random_index =
              Random.State.int Random.State.default (List.length term_list)
            in
            let random_term = List.nth_exn term_list random_index in
            let result =
              { random_term with color = random_term.color + color_value }
              :: result
            in
            let result_sum =
              List.fold result ~init:0 ~f:(fun acc x -> acc + x.value)
            in
            let remaining_terms =
              List.filteri term_list ~f:(fun i _ -> i <> random_index)
            in
            if result_sum = target then
              List.sort (result @ remaining_terms) ~compare:(fun a b ->
                  compare a.index b.index)
            else if result_sum > target then inner target [] terms
            else inner target result remaining_terms
      in
      inner target [] terms
  in
  List.mapi sequence ~f:(fun index value -> { index; value; color = 0 })
  |> get_rando_seq time.hour 1
  |> get_rando_seq time.minute 2
  |> List.map ~f:(fun x -> (x.color, x.value))

let rec show_add_time seq l =
  let eq x y = x = y in
  let first_two = List.sub seq ~pos:0 ~len:2 in
  let one_two_list = [ 1; 2 ] in
  let ret_val t i = (t.color, i, t.value) in
  let map_first_two =
    List.mapi l ~f:(fun i x ->
        if i = 0 then ret_val x (succ i)
        else if i = 2 then ret_val x i
        else ret_val x 0)
  in
  function
  | 0 -> List.map l ~f:(fun x -> ret_val x 0)
  | 1 ->
      List.mapi l ~f:(fun i x ->
          if i < 2 then ret_val x (succ i) else ret_val x 0)
  | 2 ->
      if List.equal eq first_two one_two_list then
        List.mapi l ~f:(fun i x ->
            if i = 1 || i = 2 then ret_val x i else ret_val x 0)
      else map_first_two
  | _ ->
      if List.equal eq (List.sub seq ~pos:1 ~len:2) one_two_list then
        List.mapi l ~f:(fun i x ->
            if i = 1 then ret_val x i
            else if i = 3 then ret_val x (pred i)
            else ret_val x 0)
      else if List.equal eq first_two one_two_list then map_first_two
      else
        List.mapi l ~f:(fun i x ->
            if i = 0 then ret_val x (succ i)
            else if i = 3 then ret_val x (pred i)
            else ret_val x 0)
