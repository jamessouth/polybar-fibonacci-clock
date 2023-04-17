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

(* let pl2 f lst =
     let rec print_elements = function
       | [] -> ()
       | h :: t ->
           f h;
           print_string ";";
           print_elements t
     in
     print_string "[";
     print_elements lst;
     print_string "]";
     print_newline ()

   let pint2 = pl2 print_int *)

let rec sum = function [] -> 0 | h :: t -> h.value + sum t
let ddd l = List.mapi (fun index value -> { index; value; color = 0 }) l

let get_rando_seq n t l =
  let rec inner n res ll =
    match ll with
    | [] -> failwith "n cannot be reached with these numbers"
    | _ ->
        let r = Random.int (List.length ll) in
        let randterm = List.nth ll r in
        let res =
          { randterm with color = (randterm.color + if t = "h" then 1 else 2) }
          :: res
        in
        let y = sum res in
        let nnn = List.filteri (fun i _x -> i <> r) ll in
        if y = n then List.sort (fun a b -> compare a.index b.index) (res @ nnn)
        else if y > n then inner n [] l
        else inner n res nnn
  in
  inner n [] l
