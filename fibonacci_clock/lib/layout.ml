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
  |> get_rando_seq time.hour 1 |> get_rando_seq time.minute 2
  |> List.map ~f:(fun x -> (x.color, x.value))



let rec show_add_time at i l = if at = 0 then l else let z = (List.take l i) in let w = (List.fold z ~init:0 ~f:(fun acc x -> acc + x)) in if w > at then (List.tl_exn z) else if w = at then z else show_add_time at (i+1) l;;


