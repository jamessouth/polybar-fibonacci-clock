type t = { color : int; value : int; index : int }

let pl f lst =
  let rec print_elements = function
    | [] -> ()
    | h :: t ->
        print_string "(";
        f (fst h);
        print_string ", ";
        f (snd h);
        print_string "); ";
        print_elements t
  in
  print_string "[";
  print_elements lst;
  print_string "]";
  print_newline ()

let _pint = pl print_int

let get_layout hour minute sequence =
  let get_term_list nums =
    List.mapi (fun index value -> { index; value; color = 0 }) nums
  in
  let get_rando_seq target color_value terms =
    let rec inner target result term_list =
      match term_list with
      | [] -> failwith "target cannot be reached with these numbers"
      | _ ->
          let rec sum = function [] -> 0 | hd :: tl -> hd.value + sum tl in
          let random_index = Random.int (List.length term_list) in
          let random_term = List.nth term_list random_index in
          let result =
            { random_term with color = random_term.color + color_value }
            :: result
          in
          let result_sum = sum result in
          let remaining_terms =
            List.filteri (fun i _x -> i <> random_index) term_list
          in
          if result_sum = target then
            List.sort
              (fun a b -> compare a.index b.index)
              (result @ remaining_terms)
          else if result_sum > target then inner target [] terms
          else inner target result remaining_terms
    in
    inner target [] terms
  in
  get_term_list sequence |> get_rando_seq hour 1 |> get_rando_seq minute 2
  |> List.map (fun x -> (x.color, x.value))
