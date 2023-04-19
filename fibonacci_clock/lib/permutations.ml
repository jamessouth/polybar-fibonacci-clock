type term = { color : int; value : int; index : int }

let pl f lst =
  let rec print_elements = function
    | [] -> ()
    | h :: t ->
        print_string "{c:";
        f h.color;
        print_string " v:";
        f h.value;
        print_string " i:";
        f h.index;
        print_string "} ; ";
        print_elements t
  in
  print_string "[";
  print_elements lst;
  print_string "]";
  print_newline ()

let pint = pl print_int
let rec sum = function [] -> 0 | hd :: tl -> hd.value + sum tl

let get_term_list l =
  List.mapi (fun index value -> { index; value; color = 0 }) l

let get_rando_seq target period l =
  let rec inner target result ll =
    match ll with
    | [] -> failwith "target cannot be reached with these numbers"
    | _ ->
        let random_index = Random.int (List.length ll) in
        let random_term = List.nth ll random_index in
        let result =
          {
            random_term with
            color =
              (random_term.color
              +
              match period with
              | "hour" -> 1
              | "minute" -> 2
              | _ -> failwith "period must be \"hour\" or \"minute\"");
          }
          :: result
        in
        let result_sum = sum result in
        let remaining_terms = List.filteri (fun i _x -> i <> random_index) ll in
        if result_sum = target then
          List.sort
            (fun a b -> compare a.index b.index)
            (result @ remaining_terms)
        else if result_sum > target then inner target [] l
        else inner target result remaining_terms
  in
  inner target [] l
